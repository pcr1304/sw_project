(* ══════════════════════════════════════════════════════════════════
   ProjX v4  —  bin/main.ml

   Usage:
     dune exec bin/main.exe -- input/queries.px
     dune exec bin/main.exe -- input/queries.px out.html

   Image assets are looked for in  <dir-of-input-file>/assets/
   e.g.  input/assets/mars.png
         input/assets/jupiter.png
         input/assets/saturn.png
         input/assets/planet1.jpeg
         input/assets/bgearth.jpeg

   If an image is not found a warning is printed and the game falls
   back to a procedural planet globe (the existing canvas.js behaviour).
   ══════════════════════════════════════════════════════════════════ *)

open Projx

let explode s = List.init (String.length s) (String.get s)

(* ── base64 encoder ─────────────────────────────────────────────── *)
let base64_table =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let base64_encode (bytes : bytes) : string =
  let n = Bytes.length bytes in
  let buf = Buffer.create (((n / 3) + 1) * 4) in
  let i = ref 0 in
  while !i + 2 < n do
    let b0 = Char.code (Bytes.get bytes !i) in
    let b1 = Char.code (Bytes.get bytes (!i + 1)) in
    let b2 = Char.code (Bytes.get bytes (!i + 2)) in
    Buffer.add_char buf base64_table.[b0 lsr 2];
    Buffer.add_char buf base64_table.[((b0 land 3) lsl 4) lor (b1 lsr 4)];
    Buffer.add_char buf base64_table.[((b1 land 15) lsl 2) lor (b2 lsr 6)];
    Buffer.add_char buf base64_table.[b2 land 63];
    i := !i + 3
  done;
  if !i + 1 = n then begin
    let b0 = Char.code (Bytes.get bytes !i) in
    Buffer.add_char buf base64_table.[b0 lsr 2];
    Buffer.add_char buf base64_table.[(b0 land 3) lsl 4];
    Buffer.add_string buf "=="
  end
  else if !i + 2 = n then begin
    let b0 = Char.code (Bytes.get bytes !i) in
    let b1 = Char.code (Bytes.get bytes (!i + 1)) in
    Buffer.add_char buf base64_table.[b0 lsr 2];
    Buffer.add_char buf base64_table.[((b0 land 3) lsl 4) lor (b1 lsr 4)];
    Buffer.add_char buf base64_table.[(b1 land 15) lsl 2];
    Buffer.add_char buf '='
  end;
  Buffer.contents buf

(* Read a file as bytes, return None if missing *)
let read_bytes path =
  match open_in_bin path with
  | ic ->
      let n = in_channel_length ic in
      let buf = Bytes.create n in
      really_input ic buf 0 n;
      close_in ic;
      Some buf
  | exception Sys_error _ -> None

(* Build a data-URI string for an image *)
let data_uri mime path =
  match read_bytes path with
  | None ->
      Printf.eprintf
        "[warn] image not found: %s  (game will use procedural fallback)\n" path;
      "null"
  | Some bytes ->
      Printf.eprintf "[info] loaded image: %s\n" path;
      Printf.sprintf "\"data:%s;base64,%s\"" mime (base64_encode bytes)

(* ── simple string replace (no external libs) ───────────────────── *)
let replace_all needle replacement haystack =
  let buf = Buffer.create (String.length haystack) in
  let nlen = String.length needle in
  let hlen = String.length haystack in
  let i = ref 0 in
  while !i <= hlen - nlen do
    if String.sub haystack !i nlen = needle then begin
      Buffer.add_string buf replacement;
      i := !i + nlen
    end
    else begin
      Buffer.add_char buf haystack.[!i];
      i := !i + 1
    end
  done;
  while !i < hlen do
    Buffer.add_char buf haystack.[!i];
    i := !i + 1
  done;
  Buffer.contents buf

(* ══════════════════════════════════════════════════════════════════
   HTML / JS template
   Placeholders replaced at runtime:
     __PROJX_DATA__     <- JSON array from json_emit
     __IMG_MARS__       <- data-URI or null
     __IMG_JUPITER__    <- data-URI or null
     __IMG_SATURN__     <- data-URI or null
     __IMG_MOON__       <- data-URI or null
     __IMG_EARTH__      <- data-URI or null
   ══════════════════════════════════════════════════════════════════ *)
let html_template =
  {html|<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>ProjX v4 | Simulation Lab</title>
<link href="https://fonts.googleapis.com/css2?family=Space+Grotesk:wght@300;400;500;600;700&family=Orbitron:wght@700;900&family=Fira+Code:wght@300;400;500;600&display=swap" rel="stylesheet">
<style>
:root{
  --bg:#060810;--surface:#0b0e18;--panel:#0f1220;--border:#1c2333;
  --glow:#00ffe7;--glow2:#ff4d6d;--yellow:#ffe156;--dim:#5a6480;
  --text:#d0daf0;--green:#39ff8f;--orange:#ff9f43;--purple:#b39dff;
  --font-ui:'Space Grotesk',sans-serif;
  --font-logo:'Orbitron',sans-serif;
  --font-mono:'Fira Code',monospace;
}
body.light{
  --bg:#eef2fc;--surface:#ffffff;--panel:#f4f7ff;--border:#c4cde4;
  --glow:#0055bb;--glow2:#cc1133;--yellow:#b06000;--dim:#5060a0;
  --text:#0e1530;--green:#006633;--orange:#b84400;--purple:#4422bb;
}
/* ── light mode structural overrides ── */
body.light header{
  background:rgba(238,242,252,.98);border-bottom:1px solid var(--border);
}
body.light .logo{color:var(--text);}
body.light .logo em{color:var(--glow);text-shadow:none;}
body.light .tab-btn{color:#4a5580;border-color:var(--border);}
body.light .tab-btn:hover{border-color:var(--glow);color:var(--glow);}
body.light .tab-btn.active{
  border-color:var(--glow);color:var(--glow);
  background:rgba(0,85,187,.07);box-shadow:0 0 6px rgba(0,85,187,.15);
}
body.light .tab-btn.fork-tab.active{
  border-color:var(--purple);color:var(--purple);
  background:rgba(68,34,187,.06);box-shadow:none;
}
body.light .tab-btn.game-tab.active{
  border-color:var(--yellow);color:var(--yellow);
  background:rgba(176,96,0,.06);box-shadow:none;
}
body.light .toggle-btn{color:#4a5580;border-color:var(--border);}
body.light .toggle-btn:hover{border-color:var(--purple);color:var(--purple);}
body.light .toggle-btn.active3d{
  border-color:var(--purple);color:var(--purple);
  background:rgba(68,34,187,.08);box-shadow:none;
}
body.light .run-btn{
  background:var(--glow);color:#fff;
  box-shadow:0 0 12px rgba(0,85,187,.3);
}
body.light .run-btn:hover{background:#003d99;box-shadow:0 0 18px rgba(0,85,187,.45);}
body.light .canvas-container{background:var(--surface);border-color:var(--border);}
body.light .panel{background:var(--panel);border-color:var(--border);}
body.light .panel-title{color:var(--glow);}
body.light .panel-title::before{background:var(--glow);box-shadow:none;}
body.light .q-card{background:#e8edf8;border-left-color:var(--glow);}
body.light .q-card.pass{border-left-color:var(--green);}
body.light .q-card.fail{border-left-color:var(--glow2);}
body.light .q-label{color:var(--dim);}
body.light .q-value{color:var(--green);}
body.light .q-value.fail{color:var(--glow2);}
body.light .q-unit{color:var(--dim);}
body.light .q-note{color:var(--orange);}
body.light .env-key{color:var(--dim);}
body.light .env-val{color:var(--yellow);}
body.light .legend-item{color:var(--text);}
body.light #tooltip{
  background:rgba(255,255,255,.97);border-color:var(--glow);
  box-shadow:0 4px 18px rgba(0,0,0,.15),0 0 8px rgba(0,85,187,.1);
  color:var(--text);
}
body.light .anim-progress-bar{background:var(--glow);box-shadow:none;}
body.light .crosshair-h{background:rgba(0,85,187,.18);}
body.light .crosshair-v{background:rgba(0,85,187,.18);}
body.light .hint-3d{background:rgba(238,242,252,.9);border-color:var(--border);color:var(--dim);}
/* ── dark mode: improve dim-text legibility ── */
body:not(.light) .env-key{color:#7a8aaa;}
body:not(.light) .q-label{color:#7a8aaa;}
body:not(.light) .q-unit{color:#7a8aaa;}
body:not(.light) .legend-item{color:#c0cce8;}
body:not(.light) .panel-title{color:#44ddcc;}
*,*::before,*::after{box-sizing:border-box;margin:0;padding:0}
body{
  background:var(--bg);color:var(--text);
  font-family:var(--font-ui);
  height:100vh;display:flex;flex-direction:column;overflow:hidden;
  background-image:radial-gradient(ellipse 80% 50% at 50% -10%,rgba(0,255,231,.06) 0%,transparent 60%);
}
header{
  display:flex;align-items:center;justify-content:space-between;
  padding:10px 20px;border-bottom:1px solid var(--border);
  background:rgba(8,10,15,.97);z-index:20;flex-shrink:0;gap:12px;
}
.logo{font-family:var(--font-logo);font-size:1.2rem;font-weight:900;letter-spacing:3px;color:#fff;flex-shrink:0}
.logo em{color:var(--glow);font-style:normal;text-shadow:0 0 14px var(--glow)}
.logo sup{font-size:.45rem;color:var(--dim);letter-spacing:0}
#tabs{display:flex;gap:4px;overflow-x:auto;flex:1;padding-bottom:2px;scrollbar-width:none}
#tabs::-webkit-scrollbar{display:none}
.tab-btn{flex-shrink:0;background:transparent;border:1px solid var(--border);color:var(--dim);
  padding:5px 13px;cursor:pointer;border-radius:4px;font-size:.72rem;
  font-family:var(--font-ui);font-weight:500;letter-spacing:.3px;transition:all .18s;white-space:nowrap}
.tab-btn:hover{border-color:var(--glow);color:var(--glow)}
.tab-btn.active{border-color:var(--glow);color:var(--glow);background:rgba(0,255,231,.08);box-shadow:0 0 8px rgba(0,255,231,.2)}
.tab-btn.fork-tab.active{border-color:var(--purple);color:var(--purple);background:rgba(179,157,255,.08);box-shadow:0 0 8px rgba(179,157,255,.2)}
.tab-btn.game-tab.active{border-color:var(--yellow);color:var(--yellow);background:rgba(255,225,86,.08);box-shadow:0 0 8px rgba(255,225,86,.2)}
.toggle-btn{flex-shrink:0;background:transparent;border:1px solid var(--border);color:var(--dim);
  padding:7px 16px;border-radius:4px;font-size:.72rem;letter-spacing:.5px;
  font-family:var(--font-ui);font-weight:600;cursor:pointer;transition:all .18s;}
.toggle-btn:hover{border-color:var(--purple);color:var(--purple);}
.toggle-btn.active3d{border-color:var(--purple);color:var(--purple);
  background:rgba(179,157,255,.1);box-shadow:0 0 10px rgba(179,157,255,.3);}
/* ── 3D toggle button — distinct purple accent so it's always easy to spot ── */
#toggleDim{
  border-color:rgba(179,157,255,.7);color:var(--purple);
  background:rgba(179,157,255,.1);
  padding:7px 22px;font-size:.76rem;letter-spacing:1.8px;
  box-shadow:0 0 12px rgba(179,157,255,.25),inset 0 0 0 1px rgba(179,157,255,.08);
}
#toggleDim:hover{
  border-color:var(--purple);background:rgba(179,157,255,.2);
  box-shadow:0 0 20px rgba(179,157,255,.5),inset 0 0 0 1px rgba(179,157,255,.15);
  color:var(--purple);
}
#toggleDim.active3d{
  border-color:var(--purple);background:rgba(179,157,255,.22);
  box-shadow:0 0 24px rgba(179,157,255,.6),inset 0 0 0 1px rgba(179,157,255,.2);
  color:var(--purple);
}
body.light #toggleDim{
  border-color:rgba(68,34,187,.55);color:var(--purple);
  background:rgba(68,34,187,.07);
  box-shadow:0 0 10px rgba(68,34,187,.15);
}
body.light #toggleDim:hover{
  background:rgba(68,34,187,.14);box-shadow:0 0 16px rgba(68,34,187,.28);
}
body.light #toggleDim.active3d{
  background:rgba(68,34,187,.14);box-shadow:0 0 18px rgba(68,34,187,.32);
}
.run-btn{background:var(--glow);color:#000;border:none;padding:7px 18px;border-radius:4px;
  font-family:var(--font-logo);font-size:.62rem;font-weight:700;letter-spacing:1.5px;
  cursor:pointer;transition:all .18s;box-shadow:0 0 16px rgba(0,255,231,.4);flex-shrink:0}
.run-btn:hover{background:#fff;box-shadow:0 0 26px rgba(0,255,231,.7);transform:scale(1.04)}
.run-btn:disabled{opacity:.4;cursor:not-allowed;transform:none}
main{display:flex;flex:1;overflow:hidden}
.viewport-wrap{flex:1;display:flex;flex-direction:column;padding:14px 0 14px 14px;min-width:0}
.canvas-container{flex:1;position:relative;background:var(--surface);
  border:1px solid var(--border);border-radius:6px;overflow:hidden}
canvas{width:100%;height:100%;display:block}
.canvas-container::after{content:'';position:absolute;inset:0;
  background:repeating-linear-gradient(0deg,transparent,transparent 2px,rgba(0,0,0,.03) 2px,rgba(0,0,0,.03) 4px);
  pointer-events:none;border-radius:6px;z-index:6}
.canvas-label{position:absolute;top:10px;left:14px;font-size:.62rem;color:var(--dim);letter-spacing:1px;pointer-events:none;z-index:7}
.sidebar{width:252px;flex-shrink:0;display:flex;flex-direction:column;padding:14px;gap:10px;
  overflow-y:auto;scrollbar-width:thin;scrollbar-color:var(--border) transparent}
.sidebar::-webkit-scrollbar{width:3px}
.sidebar::-webkit-scrollbar-thumb{background:var(--border);border-radius:2px}
.panel{background:var(--panel);border:1px solid var(--border);border-radius:6px;padding:12px}
.panel-title{font-size:.62rem;color:var(--glow);letter-spacing:2px;text-transform:uppercase;
  margin-bottom:9px;padding-bottom:5px;border-bottom:1px solid var(--border);
  font-family:var(--font-ui);font-weight:700;
  display:flex;align-items:center;gap:6px}
.panel-title::before{content:'';display:inline-block;width:5px;height:5px;border-radius:50%;
  background:var(--glow);box-shadow:0 0 6px var(--glow);flex-shrink:0}
.q-card{background:var(--bg);border-left:2px solid var(--glow);border-radius:0 4px 4px 0;
  padding:7px 10px;margin-bottom:5px}
.q-card:last-child{margin-bottom:0}
.q-card.pass{border-left-color:var(--green)}
.q-card.fail{border-left-color:var(--glow2)}
.q-card.col{border-left-color:var(--orange)}
.q-label{font-size:.6rem;color:var(--dim);text-transform:uppercase;letter-spacing:.5px;margin-bottom:2px;word-break:break-all;font-family:var(--font-ui);font-weight:500}
.q-value{font-size:.9rem;color:var(--green);font-weight:700;font-family:var(--font-mono)}
.q-value.fail{color:var(--glow2)}
.q-unit{font-size:.65rem;color:var(--dim);margin-left:3px;font-family:var(--font-ui)}
.q-note{font-size:.6rem;color:var(--orange);margin-top:3px;word-break:break-word;font-family:var(--font-mono)}
.env-row{display:flex;justify-content:space-between;align-items:center;padding:4px 0;
  border-bottom:1px solid var(--border);font-size:.72rem}
.env-row:last-child{border-bottom:none}
.env-key{color:var(--dim);font-weight:500}.env-val{color:var(--yellow);font-family:var(--font-mono);font-size:.68rem}
#legend{display:flex;flex-wrap:wrap;gap:7px;padding:4px 0 0}
.legend-item{display:flex;align-items:center;gap:5px;font-size:.68rem;color:var(--text);font-weight:500}
.legend-dot{width:14px;height:3px;border-radius:2px}
#tooltip{position:fixed;background:rgba(6,8,16,.97);border:1px solid var(--glow);
  border-radius:5px;padding:8px 12px;font-size:.7rem;pointer-events:none;display:none;
  z-index:1000;box-shadow:0 4px 20px rgba(0,0,0,.6),0 0 10px rgba(0,255,231,.15);
  min-width:120px;font-family:var(--font-mono)}
#tooltip .tt-id{font-size:.64rem;margin-bottom:3px;font-family:var(--font-ui);font-weight:600}
#tooltip .tt-row{color:var(--text);margin:1px 0}
#tooltip .tt-row span{color:var(--yellow)}
.anim-progress{position:absolute;bottom:0;left:0;right:0;height:2px;
  background:rgba(0,255,231,.1);border-radius:0 0 6px 6px;overflow:hidden;z-index:8}
.anim-progress-bar{height:100%;background:var(--glow);box-shadow:0 0 8px var(--glow);
  transition:width .05s linear;width:0%}
.crosshair-h,.crosshair-v{position:absolute;pointer-events:none;opacity:0;transition:opacity .1s;z-index:3}
.crosshair-h{left:0;right:0;height:1px;background:rgba(0,255,231,.13)}
.crosshair-v{top:0;bottom:0;width:1px;background:rgba(0,255,231,.13)}
/* ── 3D overlay ── */
#threejsContainer{position:absolute;top:0;left:0;width:100%;height:100%;display:none;z-index:2;}
.hint-3d{position:absolute;bottom:30px;left:50%;transform:translateX(-50%);
  font-size:.57rem;color:var(--dim);letter-spacing:1px;pointer-events:none;z-index:9;
  white-space:nowrap;display:none;background:rgba(8,10,15,.75);
  padding:3px 12px;border-radius:3px;border:1px solid var(--border);}
/* ── plot selector chips ── */
#plotSelector{position:absolute;top:10px;right:14px;z-index:8;display:none;gap:4px;flex-wrap:wrap}
.plot-sel-btn{flex-shrink:0;background:rgba(0,0,0,.45);border:1px solid var(--border);color:var(--dim);
  padding:3px 10px;border-radius:3px;font-size:.6rem;letter-spacing:.5px;cursor:pointer;
  font-family:var(--font-ui);font-weight:500;transition:all .15s;backdrop-filter:blur(4px)}
.plot-sel-btn:hover{border-color:var(--glow);color:var(--glow)}
.plot-sel-btn.psel-active{border-color:var(--glow);color:var(--glow);background:rgba(0,255,231,.1);box-shadow:0 0 6px rgba(0,255,231,.18)}
body.light #plotSelector .plot-sel-btn{background:rgba(255,255,255,.75);border-color:var(--border);color:#4a5580}
body.light #plotSelector .plot-sel-btn.psel-active{border-color:var(--glow);color:var(--glow);background:rgba(0,85,187,.07)}
</style>
</head>
<body>
<div id="tooltip">
  <div class="tt-id" id="tt-id"></div>
  <div class="tt-row">X: <span id="tt-x"></span> m</div>
  <div class="tt-row">Y: <span id="tt-y"></span> m</div>
  <div class="tt-row">T: <span id="tt-t"></span> s</div>
</div>
<header>
  <div class="logo">Proj<em>X</em><sup> v4</sup></div>
  <div id="tabs"></div>
  <button class="toggle-btn" id="toggleDim" onclick="toggleDimension()">3D</button>
  <button class="toggle-btn" id="toggleTheme" onclick="toggleTheme()" title="Toggle light/dark theme">☀</button>
  <button class="run-btn" id="runBtn" onclick="startAnimation()">&#9654; RUN</button>
</header>
<main>
  <div class="viewport-wrap">
    <div class="canvas-container" id="canvasContainer">
      <div class="canvas-label" id="canvasLabel">TRAJECTORY PLOT</div>
      <div id="plotSelector"></div>
      <canvas id="mainCanvas"></canvas>
      <div id="threejsContainer"></div>
      <div class="hint-3d" id="hint3d">⟲ drag to orbit &nbsp;·&nbsp; ⊕ scroll to zoom &nbsp;·&nbsp; ▶ RUN to animate</div>
      <div class="anim-progress"><div class="anim-progress-bar" id="progressBar"></div></div>
      <div class="crosshair-h" id="chH"></div>
      <div class="crosshair-v" id="chV"></div>
    </div>
  </div>
  <aside class="sidebar">
    <div class="panel"><div class="panel-title">Environment</div><div id="envContent"></div></div>
    <div class="panel"><div class="panel-title">Metrics</div><div id="metricsContent"></div></div>
    <div class="panel"><div class="panel-title" id="legendTitle">Trajectories</div><div id="legend"></div></div>
  </aside>
</main>
<script src="https://cdnjs.cloudflare.com/ajax/libs/three.js/r128/three.min.js"></script>
<script>
/* ── injected data ── */
const _RAW_DATA   = __PROJX_DATA__;
const IMG_MARS    = __IMG_MARS__;
const IMG_JUPITER = __IMG_JUPITER__;
const IMG_SATURN  = __IMG_SATURN__;
const IMG_MOON    = __IMG_MOON__;
const IMG_EARTH   = __IMG_EARTH__;

/* ── palette ── */
const PALETTE=['#00ffe7','#ff4d6d','#ffe156','#39ff8f','#ff9f43','#a29bfe',
               '#74b9ff','#fd79a8','#55efc4','#fdcb6e','#e17055','#6c5ce7'];
/* darker, saturated colours for light backgrounds */
const LIGHT_PALETTE=['#0055cc','#cc1133','#8b6000','#006633','#b84400','#4422bb',
                     '#006699','#bb2266','#007755','#cc7700','#aa4422','#5533aa'];
const _darkMap={};const _lightMap={};let _di=0,_li=0;
const colorFor=id=>{
  if(isLightTheme()){
    if(!_lightMap[id])_lightMap[id]=LIGHT_PALETTE[(_li++)%LIGHT_PALETTE.length];
    return _lightMap[id];
  }else{
    if(!_darkMap[id])_darkMap[id]=PALETTE[(_di++)%PALETTE.length];
    return _darkMap[id];
  }
};

/* fork branch colours — each planet gets its own distinct colour */
const FORK_COLORS={
  earth:'#4fc3f7',moon:'#cfd8dc',mars:'#ef5350',
  jupiter:'#ffb74d',saturn:'#c8a060',sun:'#ffd54f'
};
const forkColor=label=>{
  const k=label.toLowerCase();
  for(const[p,c]of Object.entries(FORK_COLORS))if(k.includes(p))return c;
  return PALETTE[6];
};

/* ── merge consecutive Fork: tabs into one multi-branch object ──
   Only groups that are DIRECTLY consecutive (no other tab type in between)
   are merged, preserving the exact declaration order from the input file. ── */
function mergeForksInData(arr){
  const out=[];let i=0;
  while(i<arr.length){
    const s=arr[i];
    if(s.label&&s.label.match(/^Fork:/)){
      const grp=[s];let j=i+1;
      while(j<arr.length&&arr[j].label&&arr[j].label.startsWith('Fork:'))grp.push(arr[j++]);
      const projs=grp.map(g2=>{
        const bl=(g2.label.match(/Fork:\s*(\S.*?)\s*\(/)||g2.label.match(/Fork:\s*(\S+)/)||[,'?'])[1];
        const p=g2.projectiles&&g2.projectiles[0]?{...g2.projectiles[0]}:
          {id:'p',angle:45,azimuth:0,speed:50,launch_from:[0,0],mass:null,drag_coeff:null,cross_section:null};
        p.branchLabel=bl; p.branchGravity=g2.gravity; p.branchColor=forkColor(bl);
        return p;
      });
      const allBounces=grp.flatMap(g2=>(g2.bounces||[]).map(b=>({...b,branchGravity:g2.gravity})));
      out.push({mode:'fork',
        label:'Fork: '+grp.map(g2=>(g2.label.match(/Fork:\s*(\S.*?)\s*\(/)||g2.label.match(/Fork:\s*(\S+)/)||[,'?'])[1]).join(' | '),
        gravity:grp[0].gravity,air_resistance:false,air_density:1.225,wind_x:0,wind_y:0,wind_z:0,
        projectiles:projs,bounces:allBounces,annotations:[],collisions:[],queries:[]});
      i=j;
    }else{out.push(s);i++;}
  }
  return out;
}
const DATA=mergeForksInData(_RAW_DATA);

/* ── planet config ── */
const PLANETS={
  earth :{skyA:'#071320',skyB:'#1a4a6b',gndA:'#2d5a1b',gndB:'#1a3a0d',gndLine:'#3d7a25',stars:false,emoji:'🌍',accent:'#4fc3f7'},
  moon  :{skyA:'#000005',skyB:'#0a0a18',gndA:'#888888',gndB:'#666',   gndLine:'#aaaaaa',stars:true, emoji:'🌕',accent:'#cfd8dc'},
  mars  :{skyA:'#180600',skyB:'#7a3018',gndA:'#7a3520',gndB:'#5a2010',gndLine:'#a04828',stars:false,emoji:'🔴',accent:'#ef5350'},
  jupiter:{skyA:'#0a0700',skyB:'#2e1e00',gndA:'#5c3d00',gndB:'#3a2500',gndLine:'#8b5e00',stars:true,emoji:'🟠',accent:'#ffb74d'},
  saturn:{skyA:'#0a0808',skyB:'#161010',gndA:'#1a1612',gndB:'#0a100e',gndLine:'#c8a060',stars:true, emoji:'🪐',accent:'#c8a060'},
  sun   :{skyA:'#160800',skyB:'#cc4400',gndA:'#cc4400',gndB:'#882200',gndLine:'#ff6600',stars:false,emoji:'☀️',accent:'#ffd54f'},
};
const getPlanet=n=>PLANETS[(n||'').toLowerCase()]||
  {skyA:'#0a0a1a',skyB:'#1a1a3a',gndA:'#1a2a1a',gndB:'#0a1a0a',gndLine:'#2a4a2a',stars:true,emoji:'🌍',accent:'#00ffe7'};

/* ── planet images from base64 ── */
const PLANET_IMGS={};
(function(){
  const map={mars:IMG_MARS,jupiter:IMG_JUPITER,saturn:IMG_SATURN,moon:IMG_MOON,earth:IMG_EARTH};
  for(const[name,src]of Object.entries(map)){
    if(!src)continue;
    const img=new Image();img.src=src;PLANET_IMGS[name]=img;
  }
})();

/* ── shape from drag_coeff ── */
const getShape=p=>{const cd=p?p.drag_coeff:null;if(cd==null||cd<0.6)return'sphere';if(cd<0.9)return'cuboid';return'cube';};

/* ── draw shaped object (2D) ── */
function drawObj(cx,cy,color,shape,r){
  ctx.save();ctx.shadowColor=color;ctx.shadowBlur=16;
  if(shape==='sphere'){
    const gr=ctx.createRadialGradient(cx-r*.4,cy-r*.4,r*.08,cx,cy,r);
    gr.addColorStop(0,'rgba(255,255,255,.95)');gr.addColorStop(.25,color);gr.addColorStop(1,'rgba(0,0,0,.65)');
    ctx.fillStyle=gr;ctx.beginPath();ctx.arc(cx,cy,r,0,Math.PI*2);ctx.fill();
    ctx.strokeStyle='rgba(255,255,255,.3)';ctx.lineWidth=.8;ctx.stroke();
  }else if(shape==='cube'){
    const s=r*1.7;ctx.fillStyle=color;ctx.fillRect(cx-s/2,cy-s/2,s,s);
    ctx.fillStyle='rgba(255,255,255,.22)';ctx.beginPath();ctx.moveTo(cx-s/2,cy-s/2);ctx.lineTo(cx+s/2,cy-s/2);ctx.lineTo(cx+s/2+5,cy-s/2-5);ctx.lineTo(cx-s/2+5,cy-s/2-5);ctx.closePath();ctx.fill();
    ctx.fillStyle='rgba(0,0,0,.28)';ctx.beginPath();ctx.moveTo(cx+s/2,cy-s/2);ctx.lineTo(cx+s/2,cy+s/2);ctx.lineTo(cx+s/2+5,cy+s/2-5);ctx.lineTo(cx+s/2+5,cy-s/2-5);ctx.closePath();ctx.fill();
    ctx.strokeStyle='rgba(255,255,255,.2)';ctx.lineWidth=.8;ctx.strokeRect(cx-s/2,cy-s/2,s,s);
  }else{
    const w=r*2.6,h=r*1.5;ctx.fillStyle=color;ctx.beginPath();
    if(ctx.roundRect)ctx.roundRect(cx-w/2,cy-h/2,w,h,3);else ctx.rect(cx-w/2,cy-h/2,w,h);ctx.fill();
    ctx.fillStyle='rgba(255,255,255,.18)';ctx.beginPath();
    if(ctx.roundRect)ctx.roundRect(cx-w/2,cy-h/2,w,h*.38,3);else ctx.rect(cx-w/2,cy-h/2,w,h*.38);ctx.fill();
    ctx.strokeStyle='rgba(255,255,255,.18)';ctx.lineWidth=.8;ctx.beginPath();
    if(ctx.roundRect)ctx.roundRect(cx-w/2,cy-h/2,w,h,3);else ctx.rect(cx-w/2,cy-h/2,w,h);ctx.stroke();
  }
  ctx.restore();
}

/* ── canvas / state ── */
const canvas=document.getElementById('mainCanvas');
const ctx=canvas.getContext('2d');
const tooltip=document.getElementById('tooltip');
const progressBar=document.getElementById('progressBar');
const chH=document.getElementById('chH');
const chV=document.getElementById('chV');
const runBtn=document.getElementById('runBtn');

let session=null,trajectories=[],bounceTrajs=[],collisionPts=[];
let currentPlotIdx=-1; /* -1 = trajectory view; 0+ = session.plots[i] */
const PL=58,PR=18,PT=24,PB=46;
let sx=1,sy=1,ox=0,oy=0;
let af=0,isAnim=false,animH=null,totF=1;
let is3D=false;
const toC=(wx,wy)=>({cx:ox+wx*sx,cy:oy-wy*sy});

/* ── physics: analytic + drag (2D) ── */
function buildTraj(p,g,ar,rho,windX,windY){
  const pts=[];
  let x=p.launch_from[0]||0,y=p.launch_from[1]||0;
  const rad=(p.angle||45)*Math.PI/180;
  let vx=(p.speed||30)*Math.cos(rad),vy=(p.speed||30)*Math.sin(rad);
  const dt=0.016;
  for(let t=0;t<400;t+=dt){
    if(y<-0.5&&pts.length>4)break;
    pts.push({x,y,t});
    if(ar&&p.mass&&p.drag_coeff!=null&&p.cross_section!=null){
      const dvx=vx-windX,dvy=vy-windY,v=Math.sqrt(dvx*dvx+dvy*dvy);
      const Fd=0.5*rho*p.drag_coeff*p.cross_section*v;
      vx-=(Fd*dvx/p.mass)*dt;vy-=(g+Fd*dvy/p.mass)*dt;
    }else{vy-=g*dt;}
    x+=vx*dt;y+=vy*dt;
  }
  return pts;
}
/* Convert a launch_delay (seconds) to animation frame offset.
   We advance 3 frames per requestAnimationFrame tick (af+=3) and each
   physics point is dt=0.016 s apart, so frames-per-second ≈ 3/0.016 ≈ 187.5.
   delayFrames = delay_seconds / 0.016 */
function delayToFrames(delaySec){ return Math.round((delaySec||0)/0.016); }
function buildBounceTraj(arc,g){
  const[x0,y0,angle,speed]=arc;const pts=[];let x=x0,y=y0;
  const rad=angle*Math.PI/180;let vx=speed*Math.cos(rad),vy=speed*Math.sin(rad);
  const dt=0.016;
  for(let t=0;t<400;t+=dt){if(y<-0.5&&pts.length>4)break;pts.push({x,y,t});vy-=g*dt;x+=vx*dt;y+=vy*dt;}
  return pts;
}

/* ── theme color helpers for canvas ── */
const isLightTheme=()=>document.body.classList.contains('light');
const tc=()=>isLightTheme()?{
  gridLine:'rgba(170,182,215,.65)',
  gridText:'rgba(50,65,120,.75)',
  axis:'rgba(0,75,180,.28)',
  rangeLabel:'rgba(100,70,0,.75)',
  heightLabel:'rgba(0,100,60,.7)',
  annoRange:'rgba(160,120,0,.5)',
  annoHeight:'rgba(0,140,70,.45)',
}:{
  gridLine:'rgba(30,37,53,.85)',
  gridText:'rgba(100,115,140,.9)',
  axis:'rgba(0,255,231,.22)',
  rangeLabel:'rgba(57,255,143,.6)',
  heightLabel:'rgba(57,255,143,.55)',
  annoRange:'rgba(255,225,86,.35)',
  annoHeight:'rgba(57,255,143,.28)',
};

/* ── scale / grid ── */
function computeScale(){
  let mX=10,mY=5;
  [...trajectories,...bounceTrajs].forEach(tr=>tr.points.forEach(p=>{if(p.x>mX)mX=p.x;if(p.y>mY)mY=p.y;}));
  mX*=1.1;mY*=1.15;
  const W=canvas.width-PL-PR,H=canvas.height-PT-PB;
  const s=Math.min(W/mX,H/mY);sx=s;sy=s;ox=PL;oy=canvas.height-PB;
}
const niceI=(range,tgt)=>{const r=range/tgt,m=Math.pow(10,Math.floor(Math.log10(r))),f=r/m;return f<1.5?m:f<3.5?2*m:f<7.5?5*m:10*m;};
function drawGrid(){
  const W=canvas.width,H=canvas.height,mWX=(W-PL-PR)/sx,mWY=(H-PT-PB)/sy;
  const xi=niceI(mWX,8),yi=niceI(mWY,5);
  const t=tc();
  ctx.save();ctx.font='500 10px Space Grotesk,sans-serif';
  ctx.strokeStyle=t.gridLine;ctx.lineWidth=1;
  for(let wx=0;wx<=mWX+xi;wx+=xi){const cx=ox+wx*sx;if(cx>W-PR+4)break;ctx.beginPath();ctx.moveTo(cx,PT);ctx.lineTo(cx,oy);ctx.stroke();}
  for(let wy=0;wy<=mWY+yi;wy+=yi){const cy=oy-wy*sy;if(cy<PT-4)break;ctx.beginPath();ctx.moveTo(ox,cy);ctx.lineTo(W-PR,cy);ctx.stroke();}
  ctx.fillStyle=t.gridText;
  ctx.textAlign='center';
  for(let wx=0;wx<=mWX+xi;wx+=xi){const cx=ox+wx*sx;if(cx>W-PR+4)break;ctx.fillText(wx.toFixed(0)+'m',cx,oy+16);}
  ctx.textAlign='right';
  for(let wy=0;wy<=mWY+yi;wy+=yi){const cy=oy-wy*sy;if(cy<PT-4)break;ctx.fillText(wy.toFixed(0)+'m',ox-5,cy+4);}
  ctx.strokeStyle=t.axis;ctx.lineWidth=1.5;
  ctx.beginPath();ctx.moveTo(ox,PT);ctx.lineTo(ox,oy);ctx.lineTo(W-PR,oy);ctx.stroke();
  ctx.fillStyle=t.rangeLabel;ctx.font='600 10px Space Grotesk,sans-serif';
  ctx.textAlign='center';ctx.fillText('RANGE (m)',ox+(W-PL-PR)/2,oy+32);
  ctx.fillStyle=t.heightLabel;
  ctx.save();ctx.translate(12,oy-(H-PT-PB)/2);ctx.rotate(-Math.PI/2);ctx.fillText('HEIGHT (m)',0,0);ctx.restore();
  ctx.restore();
}

/* ── draw trajectory path ── */
function drawTraj(tr,limit){
  const pts=tr.points.slice(0,limit);if(pts.length<2)return;
  ctx.save();ctx.strokeStyle=tr.color;ctx.lineWidth=tr.dashed?1.5:2.5;
  ctx.shadowColor=tr.color;ctx.shadowBlur=8;
  if(tr.dashed)ctx.setLineDash([4,4]);
  ctx.beginPath();
  pts.forEach((p,i)=>{const{cx,cy}=toC(p.x,p.y);if(i===0)ctx.moveTo(cx,cy);else ctx.lineTo(cx,cy);});
  ctx.stroke();ctx.restore();
}

function drawAnno(){
  if(!session||!session.annotations)return;
  const t=tc();
  ctx.save();
  session.annotations.forEach(a=>{
    if(a.type==='range'){
      const{cx}=toC(a.value,0);
      ctx.strokeStyle=t.annoRange;ctx.lineWidth=1;ctx.setLineDash([3,4]);
      ctx.beginPath();ctx.moveTo(cx,PT);ctx.lineTo(cx,oy);ctx.stroke();ctx.setLineDash([]);
      ctx.fillStyle=isLightTheme()?'rgba(140,100,0,.8)':'rgba(255,225,86,.7)';ctx.font='9px Share Tech Mono';
      ctx.textAlign='center';ctx.fillText('R:'+a.value.toFixed(1)+'m',cx,PT+10);
    }else if(a.type==='max_height'){
      const{cy}=toC(0,a.value);
      ctx.strokeStyle=t.annoHeight;ctx.lineWidth=1;ctx.setLineDash([3,4]);
      ctx.beginPath();ctx.moveTo(ox,cy);ctx.lineTo(canvas.width-PR,cy);ctx.stroke();ctx.setLineDash([]);
      ctx.fillStyle=isLightTheme()?'rgba(0,120,70,.85)':'rgba(57,255,143,.55)';ctx.font='9px Share Tech Mono';
      ctx.textAlign='left';ctx.fillText('H:'+a.value.toFixed(1)+'m',ox+4,cy-3);
    }
  });ctx.restore();
}

function drawCollisionMarkers(){
  collisionPts.forEach(c=>{
    const{cx,cy}=toC(c.x,c.y);
    ctx.save();ctx.strokeStyle='#ff4d6d';ctx.lineWidth=1.5;ctx.shadowColor='#ff4d6d';ctx.shadowBlur=10;
    const r=8;
    ctx.beginPath();ctx.moveTo(cx-r,cy-r);ctx.lineTo(cx+r,cy+r);ctx.stroke();
    ctx.beginPath();ctx.moveTo(cx+r,cy-r);ctx.lineTo(cx-r,cy+r);ctx.stroke();
    ctx.fillStyle='#ff4d6d';ctx.font='9px Share Tech Mono';ctx.textAlign='center';
    ctx.fillText(c.label,cx,cy-12);ctx.restore();
  });
}

/* ── sim renderer:
   - while animating: draw path up to `af` points, draw moving object at frontier
   - when done: draw full path + object at landing + annotations + collisions ── */
function renderSim(){
  canvas.width=canvas.clientWidth;canvas.height=canvas.clientHeight;
  ctx.clearRect(0,0,canvas.width,canvas.height);
  if(!session)return;
  computeScale();drawGrid();drawAnno();

  [...trajectories,...bounceTrajs].forEach(tr=>{
    const delay=tr.launchDelay||0;
    /* effective animation frame for this trajectory: subtract delay */
    const effAf=isAnim?Math.max(0,af-delay):tr.points.length;
    const lim=Math.min(effAf,tr.points.length);
    drawTraj(tr,lim);
    /* moving object at current frontier */
    const proj=(session.projectiles||[]).find(p=>p.id===tr.id)||null;
    if(isAnim&&af<delay){
      /* still in pre-launch delay: draw object sitting at launch position */
      const lp=tr.points[0];
      if(lp){const{cx,cy}=toC(lp.x,lp.y);drawObj(cx,cy,tr.color,getShape(proj),6);}
    }else{
      const oi=lim-1;
      if(oi>=0){
        const pt=tr.points[oi];
        const{cx,cy}=toC(pt.x,pt.y);
        drawObj(cx,cy,tr.color,getShape(proj),6);
      }
    }
  });

  /* launch-position marker */
  (session.projectiles||[]).forEach(p=>{
    const{cx,cy}=toC(p.launch_from[0]||0,p.launch_from[1]||0);
    drawObj(cx,cy,p.branchColor||colorFor(p.id),getShape(p),7);
  });

  /* fork: label at peak of each branch arc */
  if(session.mode==='fork'&&!isAnim){
    trajectories.forEach(tr=>{
      if(!tr.branchLabel)return;
      let pk=tr.points[0];tr.points.forEach(p=>{if(p.y>pk.y)pk=p;});
      const{cx,cy}=toC(pk.x,pk.y);
      ctx.save();ctx.fillStyle=tr.color;ctx.font='600 10px Space Grotesk,sans-serif';
      ctx.textAlign='center';ctx.shadowColor=tr.color;ctx.shadowBlur=6;
      ctx.fillText(tr.branchLabel,cx,cy-11);ctx.restore();
    });
  }

  drawCollisionMarkers();
}

/* ══════════════════════════════════════════════════════════════════
   MULTI-PLOT RENDERER
   Handles session.plots[] — additional line charts (e.g. speed vs time,
   energy vs time) produced by `plot` statements in a simulate block.
   Each plot object: { label, x_label, y_label,
     series:[{ id, color?, points:[{x,y}] }] }
══════════════════════════════════════════════════════════════════ */
function renderPlot(plot){
  canvas.width=canvas.clientWidth;canvas.height=canvas.clientHeight;
  ctx.clearRect(0,0,canvas.width,canvas.height);
  const series=plot.series||[];
  if(!series.length)return;

  /* bounds */
  let minX=Infinity,maxX=-Infinity,minY=Infinity,maxY=-Infinity;
  series.forEach(s=>s.points.forEach(p=>{
    if(p.x<minX)minX=p.x;if(p.x>maxX)maxX=p.x;
    if(p.y<minY)minY=p.y;if(p.y>maxY)maxY=p.y;
  }));
  if(!isFinite(minX))return;
  const rangeX=maxX-minX||1,rangeY=maxY-minY||1;
  const padY=rangeY*0.1;
  minY-=padY;maxY+=padY;

  const W=canvas.width-PL-PR,H=canvas.height-PT-PB;
  const scX=W/(maxX-minX),scY=H/(maxY-minY);
  const toP=(wx,wy)=>({cx:PL+(wx-minX)*scX,cy:canvas.height-PB-(wy-minY)*scY});

  /* grid */
  const t=tc();
  ctx.save();
  ctx.font='500 10px Space Grotesk,sans-serif';
  const xi=niceI(maxX-minX,8),yi=niceI(maxY-minY,5);
  ctx.strokeStyle=t.gridLine;ctx.lineWidth=1;
  for(let v=Math.ceil(minX/xi)*xi;v<=maxX+xi*.01;v+=xi){
    const cx=PL+(v-minX)*scX;if(cx>canvas.width-PR+4)break;
    ctx.beginPath();ctx.moveTo(cx,PT);ctx.lineTo(cx,canvas.height-PB);ctx.stroke();
  }
  for(let v=Math.ceil(minY/yi)*yi;v<=maxY+yi*.01;v+=yi){
    const cy=canvas.height-PB-(v-minY)*scY;if(cy<PT-4)break;
    ctx.beginPath();ctx.moveTo(PL,cy);ctx.lineTo(canvas.width-PR,cy);ctx.stroke();
  }
  ctx.fillStyle=t.gridText;
  ctx.textAlign='center';
  for(let v=Math.ceil(minX/xi)*xi;v<=maxX+xi*.01;v+=xi){
    const cx=PL+(v-minX)*scX;if(cx>canvas.width-PR+4)break;
    ctx.fillText(v.toFixed(1),cx,canvas.height-PB+16);
  }
  ctx.textAlign='right';
  for(let v=Math.ceil(minY/yi)*yi;v<=maxY+yi*.01;v+=yi){
    const cy=canvas.height-PB-(v-minY)*scY;if(cy<PT-4)break;
    ctx.fillText(v.toFixed(2),PL-5,cy+4);
  }
  ctx.strokeStyle=t.axis;ctx.lineWidth=1.5;
  ctx.beginPath();ctx.moveTo(PL,PT);ctx.lineTo(PL,canvas.height-PB);ctx.lineTo(canvas.width-PR,canvas.height-PB);ctx.stroke();
  /* axis labels */
  ctx.fillStyle=t.rangeLabel;ctx.font='600 10px Space Grotesk,sans-serif';
  ctx.textAlign='center';ctx.fillText(plot.x_label||'X',PL+W/2,canvas.height-PB+32);
  ctx.fillStyle=t.heightLabel;
  ctx.save();ctx.translate(12,canvas.height-PB-H/2);ctx.rotate(-Math.PI/2);ctx.fillText(plot.y_label||'Y',0,0);ctx.restore();
  /* title */
  ctx.fillStyle=t.gridText;ctx.font='600 11px Space Grotesk,sans-serif';
  ctx.textAlign='center';ctx.fillText(plot.label||'',PL+W/2,PT-6);
  ctx.restore();

  /* series lines */
  series.forEach(s=>{
    if(s.points.length<2)return;
    const col=s.color||colorFor(s.id||'p');
    ctx.save();ctx.strokeStyle=col;ctx.lineWidth=2.5;ctx.shadowColor=col;ctx.shadowBlur=8;
    ctx.beginPath();
    s.points.forEach((p,i)=>{const{cx,cy}=toP(p.x,p.y);if(i===0)ctx.moveTo(cx,cy);else ctx.lineTo(cx,cy);});
    ctx.stroke();ctx.restore();
  });

  /* mini legend for multiple series */
  if(series.length>1){
    const lx=PL+10,ly=PT+10;
    series.forEach((s,i)=>{
      const col=s.color||colorFor(s.id||'p');
      ctx.save();ctx.strokeStyle=col;ctx.lineWidth=2.5;ctx.shadowColor=col;ctx.shadowBlur=4;
      ctx.beginPath();ctx.moveTo(lx,ly+i*16+4);ctx.lineTo(lx+18,ly+i*16+4);ctx.stroke();
      ctx.fillStyle=col;ctx.font='500 9px Space Grotesk,sans-serif';ctx.textAlign='left';
      ctx.fillText(s.id||('series '+(i+1)),lx+22,ly+i*16+8);
      ctx.restore();
    });
  }
}

/* ── plot selector UI ── */
function setupPlotSelector(s){
  const sel=document.getElementById('plotSelector');
  sel.innerHTML='';
  const plots=(s.plots||[]).concat(
    /* also collect annotations of type 'plot' as additional charts */
    (s.annotations||[]).filter(a=>a.type==='plot')
  );
  if(!plots.length){sel.style.display='none';return;}
  sel.style.display='flex';

  const mkBtn=(label,idx)=>{
    const b=document.createElement('button');
    b.className='plot-sel-btn'+(idx===-1?' psel-active':'');
    b.textContent=label;
    b.onclick=()=>{
      currentPlotIdx=idx;
      sel.querySelectorAll('.plot-sel-btn').forEach(btn=>btn.classList.remove('psel-active'));
      b.classList.add('psel-active');
      if(idx<0){document.getElementById('canvasLabel').textContent='TRAJECTORY PLOT';renderSim();}
      else{document.getElementById('canvasLabel').textContent=(plots[idx].label||('Plot '+(idx+1))).toUpperCase();renderPlot(plots[idx]);}
    };
    return b;
  };
  sel.appendChild(mkBtn('TRAJECTORY',-1));
  plots.forEach((pl,i)=>sel.appendChild(mkBtn(pl.label||('Plot '+(i+1)),i)));
}

/* ══════════════════════════════════════════════════════════════════
   3D RENDERER  (Three.js r128)
   Handles simulate + fork modes.  Game mode always stays on 2D canvas.
══════════════════════════════════════════════════════════════════ */

let threeState=null;

/* 3D trajectory integrator — same RK4 as buildTraj but adds Z axis */
function buildTraj3D(p,g,ar,rho,windX,windY,windZ){
  const pts=[];
  let x=p.launch_from[0]||0,y=p.launch_from[1]||0,z=0;
  const elev=(p.angle||45)*Math.PI/180;
  const az=(p.azimuth||0)*Math.PI/180;
  let vx=(p.speed||30)*Math.cos(elev)*Math.cos(az);
  let vy=(p.speed||30)*Math.sin(elev);
  let vz=(p.speed||30)*Math.cos(elev)*Math.sin(az);
  const dt=0.016;
  for(let t=0;t<400;t+=dt){
    if(y<-0.5&&pts.length>4)break;
    pts.push({x,y,z,t});
    if(ar&&p.mass&&p.drag_coeff!=null&&p.cross_section!=null){
      const dvx=vx-windX,dvy=vy-windY,dvz=vz-windZ;
      const v=Math.sqrt(dvx*dvx+dvy*dvy+dvz*dvz);
      const Fd=0.5*rho*p.drag_coeff*p.cross_section*v;
      vx-=(Fd*dvx/p.mass)*dt;
      vy-=(g+Fd*dvy/p.mass)*dt;
      vz-=(Fd*dvz/p.mass)*dt;
    }else{vy-=g*dt;}
    x+=vx*dt;y+=vy*dt;z+=vz*dt;
  }
  return pts;
}

function teardown3D(){
  if(threeState){threeState.cleanup();threeState=null;}
}

const niceI3=(range,tgt)=>{const r=range/tgt,m=Math.pow(10,Math.floor(Math.log10(r||1))),f=r/m;return f<1.5?m:f<3.5?2*m:f<7.5?5*m:10*m;};

function load3D(s){
  if(typeof THREE==='undefined'){
    console.warn('[ProjX] Three.js not available — check CDN connection.');
    return;
  }
  const container=document.getElementById('threejsContainer');
  /* clear any previous Three.js canvas */
  while(container.firstChild)container.removeChild(container.firstChild);

  const W=container.clientWidth||800,H=container.clientHeight||600;

  /* ── renderer ── */
  const renderer=new THREE.WebGLRenderer({antialias:true,alpha:false});
  renderer.setPixelRatio(Math.min(window.devicePixelRatio||1,2));
  renderer.setSize(W,H);
  const lightMode=document.body.classList.contains('light');
  renderer.setClearColor(lightMode?0xeef2fc:0x080a0f,1);
  /* let CSS control canvas display size */
  renderer.domElement.style.position='absolute';
  renderer.domElement.style.top='0';renderer.domElement.style.left='0';
  renderer.domElement.style.width='100%';renderer.domElement.style.height='100%';
  container.appendChild(renderer.domElement);

  /* ── scene ── */
  const scene=new THREE.Scene();
  scene.fog=new THREE.FogExp2(lightMode?0xeef2fc:0x080a0f,0.0006);

  /* ── physics inputs ── */
  const g=s.gravity,ar=s.air_resistance||false;
  const rho=s.air_density||1.225;
  const windX=s.wind_x||0,windY=s.wind_y||0,windZ=s.wind_z||0;

  /* ── build 3D trajectories ── */
  const traj3Ds=[];
  (s.projectiles||[]).forEach(p=>{
    const pg=p.branchGravity!=null?p.branchGravity:g;
    const col=p.branchColor||colorFor(p.id);
    const anno=(s.annotations||[]).find(a=>a.type==='points3d'&&a.p===p.id);
    const pts=anno
      ?anno.value.map((pt,i)=>({x:pt[0],y:pt[1],z:pt[2],t:i*0.016}))
      :buildTraj3D(p,pg,ar,rho,windX,windY,windZ);
    const delay=delayToFrames(p.launch_delay||0);
    traj3Ds.push({id:p.id,color:col,points:pts,launchDelay:delay,proj:p});
  });

  /* ── fork fix: fan branches out along Z so they don't all overlap ── */
  if(s.mode==='fork'&&traj3Ds.length>1){
    let forkMax=10;
    traj3Ds.forEach(tr=>tr.points.forEach(pt=>{if(pt.x>forkMax)forkMax=pt.x;}));
    const zStep=forkMax*0.13;
    const zOff0=-zStep*(traj3Ds.length-1)/2;
    traj3Ds.forEach((tr,i)=>{
      const zO=zOff0+i*zStep;
      tr.points=tr.points.map(pt=>({...pt,z:pt.z+zO}));
      /* offset the launch marker too */
      if(tr.proj&&tr.proj.launch_from){
        tr.proj=Object.assign({},tr.proj,{_zOffset:zO});
      }
    });
  }

  /* ── bounce trajectories — sequential, chained after parent ── */
  const bounce3Ds=[];
  (s.bounces||[]).forEach(b=>{
    const bg=b.branchGravity!=null?b.branchGravity:g;
    const parentTr=traj3Ds.find(t=>t.id===b.p);
    let arcDelay=parentTr?((parentTr.launchDelay||0)+parentTr.points.length):0;
    (b.arcs||[]).forEach(arc=>{
      const pts2=buildBounceTraj(arc,bg).map(p=>({...p,z:0}));
      bounce3Ds.push({id:b.p+'_b',color:colorFor(b.p),
        points:pts2,launchDelay:arcDelay,proj:null});
      arcDelay+=pts2.length;
    });
  });

  /* allTrajs includes both main trajectories and bounce arcs — all get moving meshes */
  const allTrajs=[...traj3Ds,...bounce3Ds];

  /* ── scene bounds ── */
  let maxX=10,maxY=5,maxZ=1;
  allTrajs.forEach(tr=>tr.points.forEach(p=>{
    if(p.x>maxX)maxX=p.x;
    if(p.y>maxY)maxY=p.y;
    if(Math.abs(p.z)>maxZ)maxZ=Math.abs(p.z);
  }));
  const span=Math.max(maxX,maxZ);
  const objScale=Math.max(maxX,maxY,maxZ)*0.018;

  /* ── ground grid ── */
  const gridSize=span*2.6;
  const gridDivs=Math.max(10,Math.floor(gridSize/10));
  const grid=new THREE.GridHelper(gridSize,gridDivs,0x1e2535,0x0f141e);
  grid.position.set(maxX*0.5,0,maxZ*0.3);
  scene.add(grid);

  /* ── labeled axes helper ── */
  (function(){
    const axLen=Math.max(12,Math.min(50,maxX*0.22));
    /* main axis lines */
    const axDefs=[
      {dir:[1,0,0],color:0xff3333,label:'X  (range, m)'},
      {dir:[0,1,0],color:0x33dd55,label:'Y  (height, m)'},
      {dir:[0,0,1],color:0x4488ff,label:'Z  (depth, m)'},
    ];
    axDefs.forEach(({dir,color,label})=>{
      const pts=[new THREE.Vector3(0,0,0),new THREE.Vector3(dir[0]*axLen,dir[1]*axLen,dir[2]*axLen)];
      const geo=new THREE.BufferGeometry().setFromPoints(pts);
      scene.add(new THREE.Line(geo,new THREE.LineBasicMaterial({color,linewidth:3})));
      /* axis end label sprite */
      const cv=document.createElement('canvas');cv.width=200;cv.height=44;
      const cx2=cv.getContext('2d');
      cx2.fillStyle='rgba(0,0,0,0)';cx2.clearRect(0,0,200,44);
      const hex='#'+color.toString(16).padStart(6,'0');
      cx2.fillStyle=hex;cx2.font='bold 18px Arial,sans-serif';
      cx2.textAlign='left';cx2.fillText(label,4,30);
      const tex=new THREE.CanvasTexture(cv);
      const sp=new THREE.Sprite(new THREE.SpriteMaterial({map:tex,transparent:true,depthTest:false}));
      sp.position.set(dir[0]*(axLen+4),dir[1]*(axLen+4),dir[2]*(axLen+4));
      sp.scale.set(axLen*0.55,axLen*0.12,1);
      scene.add(sp);
    });
    /* scale ticks on X axis (range) */
    const worldPerTick=niceI3(maxX,5);
    for(let v=worldPerTick;v<maxX*1.05;v+=worldPerTick){
      if(v>axLen)break;
      const tg=new THREE.BufferGeometry().setFromPoints([
        new THREE.Vector3(v,-0.7,0),new THREE.Vector3(v,0.7,0)]);
      scene.add(new THREE.Line(tg,new THREE.LineBasicMaterial({color:0xff5555,linewidth:2})));
      const cv2=document.createElement('canvas');cv2.width=80;cv2.height=32;
      const c2=cv2.getContext('2d');c2.fillStyle='#ff8888';c2.font='bold 16px Arial';
      c2.textAlign='center';c2.fillText(v.toFixed(0)+'m',40,22);
      const t2=new THREE.CanvasTexture(cv2);
      const s2=new THREE.Sprite(new THREE.SpriteMaterial({map:t2,transparent:true,depthTest:false}));
      s2.position.set(v,-3,0);s2.scale.set(axLen*0.12,axLen*0.06,1);scene.add(s2);
    }
    /* scale ticks on Y axis (height) */
    for(let v=worldPerTick;v<maxY*1.05;v+=worldPerTick){
      if(v>axLen)break;
      const tg=new THREE.BufferGeometry().setFromPoints([
        new THREE.Vector3(-0.7,v,0),new THREE.Vector3(0.7,v,0)]);
      scene.add(new THREE.Line(tg,new THREE.LineBasicMaterial({color:0x44ee66,linewidth:2})));
      const cv2=document.createElement('canvas');cv2.width=80;cv2.height=32;
      const c2=cv2.getContext('2d');c2.fillStyle='#66ff88';c2.font='bold 16px Arial';
      c2.textAlign='center';c2.fillText(v.toFixed(0)+'m',40,22);
      const t2=new THREE.CanvasTexture(cv2);
      const s2=new THREE.Sprite(new THREE.SpriteMaterial({map:t2,transparent:true,depthTest:false}));
      s2.position.set(-4,v,0);s2.scale.set(axLen*0.12,axLen*0.06,1);scene.add(s2);
    }
  })();

  /* ── lights ── */
  scene.add(new THREE.AmbientLight(0xffffff,0.95));
  const dirLight=new THREE.DirectionalLight(0x00ffe7,0.3);
  dirLight.position.set(maxX,maxY*2,maxZ+20);
  scene.add(dirLight);

  /* ── static trajectory lines ── */
  allTrajs.forEach(tr=>{
    if(tr.points.length<2)return;
    const verts=new Float32Array(tr.points.length*3);
    tr.points.forEach((p,i)=>{verts[i*3]=p.x;verts[i*3+1]=p.y;verts[i*3+2]=p.z;});
    const geo=new THREE.BufferGeometry();
    geo.setAttribute('position',new THREE.BufferAttribute(verts,3));
    const mat=new THREE.LineBasicMaterial({color:new THREE.Color(tr.color),linewidth:2});
    scene.add(new THREE.Line(geo,mat));
  });

  /* ── launch-position markers ── */
  traj3Ds.forEach(tr=>{
    const p0=tr.points[0];if(!p0)return;
    const sp=new THREE.Mesh(
      new THREE.SphereGeometry(objScale*0.55,8,8),
      new THREE.MeshBasicMaterial({color:new THREE.Color(tr.color)})
    );
    sp.position.set(p0.x,p0.y,p0.z);scene.add(sp);
  });

  /* ── animated moving meshes (one per trajectory including bounce arcs) ── */
  const movingMeshes=allTrajs.map(tr=>{
    const shape=getShape(tr.proj);
    let geom;
    if(shape==='sphere')
      geom=new THREE.SphereGeometry(objScale,14,14);
    else if(shape==='cube')
      geom=new THREE.BoxGeometry(objScale*1.6,objScale*1.6,objScale*1.6);
    else
      geom=new THREE.BoxGeometry(objScale*2.2,objScale*1.2,objScale*1.2);
    const mesh=new THREE.Mesh(
      geom,
      new THREE.MeshBasicMaterial({color:new THREE.Color(tr.color)})
    );
    const p0=tr.points[0]||{x:0,y:0,z:0};
    mesh.position.set(p0.x,p0.y,p0.z);
    scene.add(mesh);
    return mesh;
  });

  /* ── camera + simple orbit controls ── */
  const camera=new THREE.PerspectiveCamera(50,W/H,0.1,20000);
  const orbDef={theta:-0.55,phi:0.95,radius:Math.max(maxX,maxY,maxZ)*1.85};
  const orb={...orbDef,dragging:false,lx:0,ly:0,
    target:new THREE.Vector3(maxX*0.5,maxY*0.2,maxZ*0.25)};

  function applyOrbit(){
    const sp=Math.sin(orb.phi),cp=Math.cos(orb.phi);
    camera.position.set(
      orb.target.x+orb.radius*sp*Math.sin(orb.theta),
      orb.target.y+orb.radius*cp,
      orb.target.z+orb.radius*sp*Math.cos(orb.theta)
    );
    camera.lookAt(orb.target);
  }
  applyOrbit();

  const onMD=e=>{orb.dragging=true;orb.lx=e.clientX;orb.ly=e.clientY;};
  const onMM=e=>{
    if(!orb.dragging)return;
    const dx=e.clientX-orb.lx,dy=e.clientY-orb.ly;
    orb.theta-=dx*0.006;
    orb.phi=Math.max(0.08,Math.min(Math.PI*0.47,orb.phi+dy*0.006));
    orb.lx=e.clientX;orb.ly=e.clientY;applyOrbit();
  };
  const onMU=()=>{orb.dragging=false;};
  const onWH=e=>{
    orb.radius=Math.max(5,orb.radius+e.deltaY*0.08);applyOrbit();
  };
  renderer.domElement.addEventListener('mousedown',onMD);
  renderer.domElement.addEventListener('mousemove',onMM);
  renderer.domElement.addEventListener('mouseup',onMU);
  renderer.domElement.addEventListener('mouseleave',onMU);
  renderer.domElement.addEventListener('wheel',onWH,{passive:true});

  /* ── reset-camera button ── */
  const resetBtn=document.createElement('button');
  resetBtn.textContent='⟲ RESET CAM';
  resetBtn.style.cssText=
    'position:absolute;top:10px;right:10px;z-index:10;'+
    'background:rgba(8,10,15,.88);border:1px solid #1e2535;color:#4a5568;'+
    'padding:4px 10px;border-radius:3px;font-family:"Share Tech Mono",monospace;'+
    'font-size:.58rem;cursor:pointer;letter-spacing:1px;transition:color .15s,border-color .15s;';
  resetBtn.onmouseover=()=>{resetBtn.style.color='var(--purple)';resetBtn.style.borderColor='var(--purple)';};
  resetBtn.onmouseout=()=>{resetBtn.style.color='#4a5568';resetBtn.style.borderColor='#1e2535';};
  resetBtn.onclick=()=>{Object.assign(orb,orbDef);applyOrbit();};
  container.appendChild(resetBtn);

  /* ── animation state (shared via closure) ── */
  const totF3D=Math.max(...allTrajs.map(t=>(t.launchDelay||0)+t.points.length),1);
  const st={animId:null,isAnim:false,af:totF3D};

  function renderLoop(){
    st.animId=requestAnimationFrame(renderLoop);
    if(st.isAnim){
      st.af+=3;
      progressBar.style.width=Math.min(st.af/totF3D*100,100)+'%';
      if(st.af>=totF3D+10){
        st.isAnim=false;st.af=totF3D;
        progressBar.style.width='100%';runBtn.disabled=false;
      }
    }
    /* move animated meshes — all trajectories including bounces */
    allTrajs.forEach((tr,i)=>{
      const delay=tr.launchDelay||0;
      const eff=Math.max(0,st.af-delay);
      const idx=Math.min(eff,tr.points.length-1);
      const pt=tr.points[idx];
      if(pt)movingMeshes[i].position.set(pt.x,pt.y,pt.z);
    });
    renderer.render(scene,camera);
  }
  renderLoop();

  threeState={
    renderer,camera,
    start:()=>{st.isAnim=true;st.af=0;runBtn.disabled=true;progressBar.style.width='0%';},
    resize:(W2,H2)=>{
      renderer.setSize(W2,H2);
      camera.aspect=W2/H2;
      camera.updateProjectionMatrix();
    },
    cleanup:()=>{
      if(st.animId)cancelAnimationFrame(st.animId);
      renderer.domElement.removeEventListener('mousedown',onMD);
      renderer.domElement.removeEventListener('mousemove',onMM);
      renderer.domElement.removeEventListener('mouseup',onMU);
      renderer.domElement.removeEventListener('mouseleave',onMU);
      renderer.domElement.removeEventListener('wheel',onWH);
      if(resetBtn.parentNode)resetBtn.parentNode.removeChild(resetBtn);
      if(renderer.domElement.parentNode)
        renderer.domElement.parentNode.removeChild(renderer.domElement);
      renderer.dispose();
    }
  };
}

function start3DAnim(){if(threeState)threeState.start();}

function toggleDimension(){
  is3D=!is3D;
  const btn=document.getElementById('toggleDim');
  if(is3D){btn.textContent='2D';btn.classList.add('active3d');}
  else{btn.textContent='3D';btn.classList.remove('active3d');}
  if(session&&session.mode!=='game')loadSession(session);
}

function toggleTheme(){
  const isLight=document.body.classList.toggle('light');
  const btn=document.getElementById('toggleTheme');
  btn.textContent=isLight?'\u263D':'\u2600';
  btn.title=isLight?'Switch to dark theme':'Switch to light theme';
  if(session)loadSession(session);
}

/* ══════════════════════════════════════════════════════════════════
   GAME — full renderGame from canvas.js (images from base64)
══════════════════════════════════════════════════════════════════ */
let gameAnimId=null;
const planetThemes={
  earth :{skyTop:'#020818',skyMid:'#0a1628',skyBot:'#162040',nebula:'rgba(56,130,245,0.04)', ground:'#1a2030',horizon:'#38d9f5'},
  moon  :{skyTop:'#0a0a0a',skyMid:'#111118',skyBot:'#1a1a22',nebula:'rgba(180,180,220,0.03)',ground:'#1c1c20',horizon:'#bbbbcc'},
  mars  :{skyTop:'#120808',skyMid:'#1e0e0a',skyBot:'#2a1510',nebula:'rgba(220,80,40,0.04)',  ground:'#1e1210',horizon:'#e87040'},
  jupiter:{skyTop:'#0c0a04',skyMid:'#1a1508',skyBot:'#28200c',nebula:'rgba(245,180,60,0.04)',ground:'#1e1a10',horizon:'#f5b040'},
  saturn:{skyTop:'#0a0808',skyMid:'#161010',skyBot:'#221a14',nebula:'rgba(200,150,100,0.04)',ground:'#1a1612',horizon:'#c8a060'},
  sun   :{skyTop:'#160800',skyMid:'#441400',skyBot:'#882200',nebula:'rgba(255,120,30,0.06)', ground:'#2a1800',horizon:'#ff8800'},
};

/* ── replay overlay (shown when game ends) ── */
function showReplayOverlay(isWin,score){
  const container=document.getElementById('canvasContainer');
  const ov=document.createElement('div');
  ov.id='replayOverlay';
  ov.style.cssText=
    'position:absolute;bottom:70px;left:50%;transform:translateX(-50%);'+
    'z-index:20;display:flex;flex-direction:column;align-items:center;gap:10px;';
  const btn=document.createElement('button');
  btn.textContent='\u21BA  PLAY AGAIN';
  const win=isWin;
  btn.style.cssText=
    'background:'+(win?'rgba(40,180,100,.15)':'rgba(200,50,80,.15)')+';'+
    'border:1px solid '+(win?'#3de87a':'#f55a6e')+';'+
    'color:'+(win?'#3de87a':'#f55a6e')+';'+
    'padding:9px 32px;border-radius:6px;'+
    'font-family:"Space Grotesk",sans-serif;font-size:.85rem;font-weight:700;'+
    'letter-spacing:1.5px;cursor:pointer;'+
    'box-shadow:0 0 16px '+(win?'rgba(61,232,122,.25)':'rgba(245,90,110,.25)')+';'+
    'transition:all .18s;';
  btn.onmouseover=()=>{btn.style.background=win?'rgba(40,180,100,.28)':'rgba(200,50,80,.28)';btn.style.transform='scale(1.05)';};
  btn.onmouseout=()=>{btn.style.background=win?'rgba(40,180,100,.15)':'rgba(200,50,80,.15)';btn.style.transform='scale(1)';};
  btn.onclick=()=>{
    const ov2=document.getElementById('replayOverlay');
    if(ov2)ov2.parentNode.removeChild(ov2);
    loadSession(session);
  };
  ov.appendChild(btn);
  container.appendChild(ov);
}

function renderGame(ctx,canvas,data){
  if(gameAnimId){cancelAnimationFrame(gameAnimId);gameAnimId=null;}
  const groundY=canvas.height-40;
  const g=data.gravity||9.8;
  const launchSpeed=data.launchSpeed||30;
  function getGameScale(){return Math.min(canvas.width/40,25);}
  const state={lives:data.lives||3,score:0,activeBullets:[]};
  let aimAngle=Math.PI/4,aimDist=0,aiming=false;
  const launcherX=50,launcherY=groundY;
  const particles=[],scorePopups=[];
  const stars=[];
  for(let i=0;i<120;i++){
    const hue=Math.random()>0.85?(180+Math.random()*60):0;
    stars.push({x:Math.random()*canvas.width,y:Math.random()*groundY*0.85,
      r:Math.random()*1.8+0.2,o:Math.random()*0.5+0.1,twinkle:0.008+Math.random()*0.04,hue});
  }
  const targets=data.targets&&data.targets.length>0?data.targets:generateTargets(data.level||1,canvas.width,groundY);
  const walls=data.walls&&data.walls.length>0?data.walls:generateWalls(data.level||1,canvas.width,groundY);
  const hitTargets=new Set();
  let frameCount=0;
  const planetName=data.planet||'earth';
  const theme=planetThemes[planetName]||planetThemes.earth;
  const floatingPlanet=PLANET_IMGS[planetName]||null;
  const planetSize=110;
  const planetCX=canvas.width*0.68;
  const planetBY=groundY*0.22;
  let mouseStartX=0,mouseStartY=0;
  function getAngleFromMouse(e){
    const r=canvas.getBoundingClientRect();
    const mx=(e.clientX-r.left)*(canvas.width/r.width);
    const my=(e.clientY-r.top)*(canvas.height/r.height);
    return Math.max(0.08,Math.min(Math.atan2(launcherY-my,mx-launcherX),Math.PI/2-0.05));
  }
  function getDragDist(e){
    const r=canvas.getBoundingClientRect();
    const mx=(e.clientX-r.left)*(canvas.width/r.width);
    const my=(e.clientY-r.top)*(canvas.height/r.height);
    return Math.sqrt((mx-mouseStartX)**2+(my-mouseStartY)**2);
  }
  let curLaunchSpeed=launchSpeed;
  const onMouseDown=(e)=>{
    if(state.lives<=0)return;
    const r=canvas.getBoundingClientRect();
    mouseStartX=(e.clientX-r.left)*(canvas.width/r.width);
    mouseStartY=(e.clientY-r.top)*(canvas.height/r.height);
    aiming=true;aimAngle=getAngleFromMouse(e);aimDist=0;
    curLaunchSpeed=Math.max(10,Math.min(aimDist/3.5,60));
  };
  const onMouseMove=(e)=>{
    if(!aiming)return;
    aimAngle=getAngleFromMouse(e);aimDist=getDragDist(e);
    curLaunchSpeed=Math.max(10,Math.min(aimDist/3.5,60));
  };
  const onMouseUp=(e)=>{
    if(!aiming)return;aiming=false;if(state.lives<=0)return;
    aimAngle=getAngleFromMouse(e);
    curLaunchSpeed=Math.max(10,Math.min(aimDist/3.5,60));
    state.activeBullets.push({x:launcherX,y:launcherY,
      vx:curLaunchSpeed*Math.cos(aimAngle),vy:curLaunchSpeed*Math.sin(aimAngle),trail:[]});
    state.lives--;
  };
  const onTouchStart=(e)=>{e.preventDefault();onMouseDown(e.touches[0]);};
  const onTouchMove=(e)=>{e.preventDefault();onMouseMove(e.touches[0]);};
  const onTouchEnd=(e)=>{e.preventDefault();onMouseUp(e.changedTouches[0]);};
  canvas.addEventListener('mousedown',onMouseDown);
  canvas.addEventListener('mousemove',onMouseMove);
  canvas.addEventListener('mouseup',onMouseUp);
  canvas.addEventListener('touchstart',onTouchStart,{passive:false});
  canvas.addEventListener('touchmove',onTouchMove,{passive:false});
  canvas.addEventListener('touchend',onTouchEnd,{passive:false});
  function spawnExplosion(x,y){
    for(let i=0;i<20;i++){
      const a=Math.random()*Math.PI*2,s=1+Math.random()*3.5;
      particles.push({x,y,vx:Math.cos(a)*s,vy:Math.sin(a)*s-1.5,
        life:40+Math.random()*20,maxLife:60,
        color:Math.random()>0.5?'#3de87a':'#f5a623',r:1.5+Math.random()*2.5});
    }
  }
  function drawEnv(){
    const sky=ctx.createLinearGradient(0,0,0,groundY);
    sky.addColorStop(0,theme.skyTop);sky.addColorStop(0.5,theme.skyMid);sky.addColorStop(1,theme.skyBot);
    ctx.fillStyle=sky;ctx.fillRect(0,0,canvas.width,groundY);
    const nt=frameCount*0.001;
    for(let i=0;i<3;i++){
      const nx=canvas.width*(0.2+i*0.3)+Math.sin(nt+i)*80;
      const ny=groundY*(0.2+i*0.15)+Math.cos(nt*0.7+i)*40;
      const nr=120+i*40;
      const ng=ctx.createRadialGradient(nx,ny,0,nx,ny,nr);
      ng.addColorStop(0,theme.nebula);ng.addColorStop(1,'rgba(0,0,0,0)');
      ctx.fillStyle=ng;ctx.fillRect(nx-nr,ny-nr,nr*2,nr*2);
    }
    stars.forEach(s=>{
      const f=Math.sin(frameCount*s.twinkle)*0.25+0.75;
      ctx.fillStyle=s.hue?`hsla(${s.hue},60%,75%,${s.o*f})`:`rgba(255,255,255,${s.o*f})`;
      ctx.beginPath();ctx.arc(s.x,s.y,s.r,0,Math.PI*2);ctx.fill();
    });
    const ox2=planetCX+Math.sin(frameCount*0.002)*50;
    const oy2=planetBY+Math.sin(frameCount*0.006)*10;
    const radius=planetSize/2;
    if(floatingPlanet&&floatingPlanet.complete&&floatingPlanet.naturalWidth>0){
      const gg=ctx.createRadialGradient(ox2,oy2,radius*0.4,ox2,oy2,radius*1.8);
      gg.addColorStop(0,theme.nebula.replace('0.04','0.15'));gg.addColorStop(1,'rgba(0,0,0,0)');
      ctx.fillStyle=gg;ctx.beginPath();ctx.arc(ox2,oy2,radius*1.8,0,Math.PI*2);ctx.fill();
      ctx.save();ctx.beginPath();ctx.arc(ox2,oy2,radius,0,Math.PI*2);ctx.clip();
      ctx.globalAlpha=0.8;
      const aspect=floatingPlanet.naturalWidth/floatingPlanet.naturalHeight;
      const drawW=Math.max(planetSize,planetSize*aspect);
      const drawH=Math.max(planetSize,planetSize/aspect);
      ctx.drawImage(floatingPlanet,ox2-drawW/2,oy2-drawH/2,drawW,drawH);
      ctx.globalAlpha=1;ctx.restore();
      ctx.beginPath();ctx.arc(ox2,oy2,radius+2,0,Math.PI*2);
      ctx.strokeStyle=theme.horizon+'30';ctx.lineWidth=1;ctx.stroke();
    }else{
      const atm=ctx.createRadialGradient(ox2,oy2,radius*0.6,ox2,oy2,radius*1.5);
      atm.addColorStop(0,'rgba(56,180,245,0.1)');atm.addColorStop(1,'rgba(0,0,0,0)');
      ctx.fillStyle=atm;ctx.beginPath();ctx.arc(ox2,oy2,radius*1.5,0,Math.PI*2);ctx.fill();
      const globe=ctx.createRadialGradient(ox2-radius*0.3,oy2-radius*0.3,0,ox2,oy2,radius);
      globe.addColorStop(0,'#4488cc');globe.addColorStop(0.4,'#2266aa');
      globe.addColorStop(0.7,'#1a4488');globe.addColorStop(1,'#0a2244');
      ctx.fillStyle=globe;ctx.beginPath();ctx.arc(ox2,oy2,radius,0,Math.PI*2);ctx.fill();
      ctx.globalAlpha=0.3;
      const cTime=frameCount*0.001;
      for(let c=0;c<4;c++){
        const cx2=ox2+Math.cos(cTime+c*1.6)*radius*0.45;
        const cy2=oy2+Math.sin(cTime*0.7+c*2)*radius*0.35;
        const cr=radius*(0.12+c*0.04);
        ctx.fillStyle='#2a8844';ctx.beginPath();ctx.arc(cx2,cy2,cr,0,Math.PI*2);ctx.fill();
      }
      ctx.globalAlpha=1;
      const shine=ctx.createRadialGradient(ox2-radius*0.35,oy2-radius*0.35,0,ox2,oy2,radius);
      shine.addColorStop(0,'rgba(255,255,255,0.15)');shine.addColorStop(0.5,'rgba(255,255,255,0)');
      ctx.fillStyle=shine;ctx.beginPath();ctx.arc(ox2,oy2,radius,0,Math.PI*2);ctx.fill();
      ctx.beginPath();ctx.arc(ox2,oy2,radius+2,0,Math.PI*2);
      ctx.strokeStyle=theme.horizon+'30';ctx.lineWidth=1;ctx.stroke();
    }
    const gGrad=ctx.createLinearGradient(0,groundY,0,canvas.height);
    gGrad.addColorStop(0,theme.ground);gGrad.addColorStop(1,'#080810');
    ctx.fillStyle=gGrad;ctx.fillRect(0,groundY,canvas.width,canvas.height-groundY);
    ctx.strokeStyle=theme.horizon+'12';ctx.lineWidth=0.5;
    for(let ry=groundY+6;ry<canvas.height;ry+=8){
      ctx.beginPath();ctx.moveTo(0,ry);
      for(let rx=0;rx<canvas.width;rx+=40)ctx.lineTo(rx+20,ry+Math.sin(rx*0.02+ry*0.1)*1.5);
      ctx.stroke();
    }
    ctx.beginPath();ctx.moveTo(0,groundY);ctx.lineTo(canvas.width,groundY);
    ctx.strokeStyle=theme.horizon;ctx.lineWidth=2;
    ctx.shadowBlur=12;ctx.shadowColor=theme.horizon;ctx.stroke();ctx.shadowBlur=0;
  }
  function drawLauncher(){
    ctx.save();ctx.translate(launcherX,launcherY);ctx.rotate(-aimAngle);
    ctx.fillStyle='rgba(0,0,0,0.3)';ctx.fillRect(2,-5,32,12);
    const bg=ctx.createLinearGradient(0,-7,0,7);
    bg.addColorStop(0,'#4ae0fa');bg.addColorStop(1,'#1a8aaa');
    ctx.fillStyle=bg;ctx.strokeStyle=theme.horizon;ctx.lineWidth=1.5;
    ctx.beginPath();ctx.roundRect(0,-7,32,14,3);ctx.fill();ctx.stroke();
    ctx.fillStyle=aiming?'#f5a623':theme.horizon;
    ctx.fillRect(30,-4,aiming?6:4,aiming?8:8);
    ctx.restore();
    ctx.beginPath();ctx.arc(launcherX,launcherY,16,Math.PI,0);
    const dg=ctx.createRadialGradient(launcherX,launcherY-4,2,launcherX,launcherY,16);
    dg.addColorStop(0,'#3a3d50');dg.addColorStop(1,'#1a1d2a');
    ctx.fillStyle=dg;ctx.fill();ctx.strokeStyle=theme.horizon;ctx.lineWidth=2;ctx.stroke();
  }
  function drawAimLine(){
    if(!aiming)return;
    ctx.setLineDash([4,6]);ctx.strokeStyle='rgba(245,166,35,0.55)';ctx.lineWidth=1.5;ctx.beginPath();
    const vx2=curLaunchSpeed*Math.cos(aimAngle),vy2=curLaunchSpeed*Math.sin(aimAngle);
    for(let i=0;i<=80;i++){
      const t=i*0.08,gs=getGameScale();
      const px2=launcherX+vx2*t*gs,py2=launcherY-(vy2*t-0.5*g*t*t)*gs;
      if(py2>launcherY+5&&i>2)break;if(px2>canvas.width)break;
      i===0?ctx.moveTo(px2,py2):ctx.lineTo(px2,py2);
    }
    ctx.stroke();ctx.setLineDash([]);
    const bx=launcherX-22,by=launcherY-90,bw=10,bh=50;
    ctx.fillStyle='rgba(0,0,0,0.5)';ctx.beginPath();ctx.roundRect(bx,by,bw,bh,3);ctx.fill();
    ctx.strokeStyle='rgba(255,255,255,0.2)';ctx.lineWidth=1;ctx.stroke();
    const pulse=0.85+Math.sin(frameCount*0.1)*0.15;
    const fill=Math.min(aimDist/200,1)*pulse;
    const pg=ctx.createLinearGradient(bx,by+bh,bx,by);
    pg.addColorStop(0,'#3de87a');pg.addColorStop(0.5,'#f5a623');pg.addColorStop(1,'#f55a6e');
    ctx.fillStyle=pg;ctx.beginPath();ctx.roundRect(bx+1,by+bh-bh*fill,bw-2,bh*fill,2);ctx.fill();
    const deg=(aimAngle*180/Math.PI).toFixed(0);
    const range=(curLaunchSpeed**2*Math.sin(2*aimAngle)/g).toFixed(0);
    ctx.fillStyle='#f5a623';ctx.font="bold 12px 'Space Grotesk',sans-serif";
    ctx.fillText(deg+'°',launcherX+40,launcherY-45);
    ctx.fillStyle='rgba(255,255,255,0.4)';ctx.font="500 10px 'Space Grotesk',sans-serif";
    ctx.fillText('~'+range+'m',launcherX+40,launcherY-30);
  }
  function drawParticles(){
    for(let i=particles.length-1;i>=0;i--){
      const p=particles[i];p.x+=p.vx;p.y+=p.vy;p.vy+=0.05;p.life--;
      if(p.life<=0){particles.splice(i,1);continue;}
      ctx.globalAlpha=p.life/p.maxLife;ctx.fillStyle=p.color;
      ctx.beginPath();ctx.arc(p.x,p.y,p.r*(p.life/p.maxLife),0,Math.PI*2);ctx.fill();
    }
    ctx.globalAlpha=1;
  }
  function drawScorePopups(){
    for(let i=scorePopups.length-1;i>=0;i--){
      const s=scorePopups[i];s.y-=1.2;s.life--;
      if(s.life<=0){scorePopups.splice(i,1);continue;}
      ctx.globalAlpha=s.life/s.maxLife;ctx.fillStyle='#3de87a';
      ctx.font="bold 18px 'Inter',sans-serif";
      ctx.shadowBlur=6;ctx.shadowColor='#3de87a';ctx.fillText(s.text,s.x,s.y);ctx.shadowBlur=0;
    }
    ctx.globalAlpha=1;
  }
  function update(){
    frameCount++;
    drawEnv();drawLauncher();drawAimLine();drawParticles();drawScorePopups();
    ctx.font="bold 14px 'JetBrains Mono',monospace";ctx.fillStyle=theme.horizon;
    ctx.fillText(planetName.toUpperCase(),15,24);
    ctx.fillStyle='rgba(255,255,255,0.4)';ctx.font="12px 'JetBrains Mono',monospace";
    ctx.fillText('g = '+g+' m/s\u00B2',15,42);
    ctx.fillStyle='#f55a6e';ctx.font="14px 'JetBrains Mono',monospace";
    ctx.shadowBlur=4;ctx.shadowColor='#f55a6e';
    ctx.fillText('LIVES: '+'\u2665 '.repeat(state.lives),canvas.width-180,24);ctx.shadowBlur=0;
    ctx.fillStyle='#3de87a';ctx.shadowBlur=4;ctx.shadowColor='#3de87a';
    ctx.fillText('SCORE: '+state.score,canvas.width-180,44);ctx.shadowBlur=0;
    if(state.activeBullets.length===0&&state.lives>0&&!aiming){
      const p=0.3+(0.5+Math.sin(frameCount*0.04)*0.5)*0.2;
      ctx.fillStyle='rgba(255,255,255,0.18)';ctx.font="13px 'Inter',sans-serif";ctx.globalAlpha=p;
      ctx.fillText('Click & drag to aim, release to fire',canvas.width/2-130,25);ctx.globalAlpha=1;
    }
    walls.forEach(w=>{
      const wg=ctx.createLinearGradient(w.x,w.y,w.x+w.w,w.y);
      wg.addColorStop(0,'#1e2030');wg.addColorStop(1,'#282c40');
      ctx.fillStyle=wg;ctx.strokeStyle=theme.horizon+'88';ctx.lineWidth=1;
      ctx.beginPath();ctx.roundRect(w.x,w.y,w.w,w.h,3);ctx.fill();ctx.stroke();
      ctx.fillStyle=theme.horizon;ctx.shadowBlur=4;ctx.shadowColor=theme.horizon;
      ctx.fillRect(w.x+1,w.y,w.w-2,2);ctx.shadowBlur=0;
      ctx.strokeStyle='rgba(255,255,255,0.04)';
      for(let ly=w.y+10;ly<w.y+w.h;ly+=10){ctx.beginPath();ctx.moveTo(w.x+2,ly);ctx.lineTo(w.x+w.w-2,ly);ctx.stroke();}
    });
    targets.forEach(t=>{
      if(hitTargets.has(t.id))return;
      const ty=t.y+Math.sin(frameCount*0.03+t.x*0.02)*15;t.renderY=ty;
      const pf=0.7+Math.sin(frameCount*0.05+t.x*0.01)*0.3;
      ctx.shadowBlur=8*pf;ctx.shadowColor='#3de87a';
      const tg=ctx.createLinearGradient(t.x,ty,t.x,ty+t.h);
      tg.addColorStop(0,`rgba(61,232,122,${0.15*pf})`);tg.addColorStop(1,`rgba(61,232,122,${0.05*pf})`);
      ctx.fillStyle=tg;ctx.strokeStyle='#3de87a';ctx.lineWidth=1.5;
      ctx.beginPath();ctx.roundRect(t.x,ty,t.w,t.h,5);ctx.fill();ctx.stroke();ctx.shadowBlur=0;
      ctx.beginPath();ctx.arc(t.x+t.w/2,ty+t.h/2,6,0,Math.PI*2);
      ctx.strokeStyle='#3de87a';ctx.lineWidth=1;ctx.stroke();
      ctx.beginPath();ctx.arc(t.x+t.w/2,ty+t.h/2,2,0,Math.PI*2);ctx.fillStyle='#3de87a';ctx.fill();
      ctx.fillStyle='#3de87a';ctx.font="bold 9px 'JetBrains Mono',monospace";ctx.fillText(t.id,t.x+3,ty-4);
    });
    for(let i=state.activeBullets.length-1;i>=0;i--){
      const b=state.activeBullets[i];
      b.trail.push({x:b.x,y:b.y});if(b.trail.length>50)b.trail.shift();
      const dt=0.06,gs=getGameScale();
      b.x+=b.vx*dt*gs;b.vy-=g*dt;b.y-=b.vy*dt*gs;
      b.trail.forEach((pt,j)=>{
        const al=(j/b.trail.length)*0.6,sz=0.5+(j/b.trail.length)*3;
        ctx.beginPath();ctx.arc(pt.x,pt.y,sz,0,Math.PI*2);
        ctx.fillStyle=`rgba(56,217,245,${al})`;ctx.fill();
      });
      ctx.beginPath();ctx.arc(b.x,b.y,5,0,Math.PI*2);ctx.fillStyle='#fff';
      ctx.shadowBlur=18;ctx.shadowColor=theme.horizon;ctx.fill();ctx.shadowBlur=0;
      ctx.beginPath();ctx.arc(b.x,b.y,8,0,Math.PI*2);
      ctx.strokeStyle=theme.horizon+'44';ctx.lineWidth=1;ctx.stroke();
      let dead=false;
      walls.forEach(w=>{
        if(b.x>=w.x&&b.x<=w.x+w.w&&b.y>=w.y&&b.y<=w.y+w.h){
          dead=true;
          for(let k=0;k<6;k++)particles.push({x:b.x,y:b.y,vx:(Math.random()-0.5)*3,vy:-Math.random()*2-1,life:20,maxLife:20,color:'#8888aa',r:1.5});
        }
      });
      if(!dead){
        targets.forEach(t=>{
          if(hitTargets.has(t.id))return;
          const ty=t.renderY||t.y;
          if(b.x>=t.x-6&&b.x<=t.x+t.w+6&&b.y>=ty-6&&b.y<=ty+t.h+6){
            hitTargets.add(t.id);state.score+=100;
            spawnExplosion(t.x+t.w/2,ty+t.h/2);
            scorePopups.push({x:t.x+2,y:ty-10,text:'+100',life:50,maxLife:50});
            dead=true;
          }
        });
      }
      if(dead||b.y>=groundY||b.x>canvas.width+20||b.x<-20)state.activeBullets.splice(i,1);
    }
    if(hitTargets.size===targets.length&&targets.length>0){
      ctx.fillStyle='rgba(10,14,20,0.82)';ctx.beginPath();ctx.roundRect(canvas.width/2-170,canvas.height/2-50,340,100,12);ctx.fill();
      ctx.strokeStyle='#3de87a55';ctx.lineWidth=1;ctx.stroke();
      ctx.fillStyle='#3de87a';ctx.font="bold 30px 'Inter',sans-serif";
      ctx.shadowBlur=14;ctx.shadowColor='#3de87a';ctx.fillText('\u2713 LEVEL CLEAR!',canvas.width/2-115,canvas.height/2+2);ctx.shadowBlur=0;
      ctx.fillStyle='rgba(255,255,255,0.45)';ctx.font="13px 'Space Grotesk',sans-serif";
      ctx.fillText('Score: '+state.score,canvas.width/2-32,canvas.height/2+26);
      if(!document.getElementById('replayOverlay'))showReplayOverlay(true,state.score);
    }else if(state.lives<=0&&state.activeBullets.length===0){
      ctx.fillStyle='rgba(10,14,20,0.82)';ctx.beginPath();ctx.roundRect(canvas.width/2-150,canvas.height/2-50,300,100,12);ctx.fill();
      ctx.strokeStyle='#f55a6e55';ctx.lineWidth=1;ctx.stroke();
      ctx.fillStyle='#f55a6e';ctx.font="bold 30px 'Inter',sans-serif";
      ctx.shadowBlur=14;ctx.shadowColor='#f55a6e';ctx.fillText('GAME OVER',canvas.width/2-95,canvas.height/2+2);ctx.shadowBlur=0;
      ctx.fillStyle='rgba(255,255,255,0.45)';ctx.font="13px 'Space Grotesk',sans-serif";
      ctx.fillText('Score: '+state.score,canvas.width/2-32,canvas.height/2+26);
      if(!document.getElementById('replayOverlay'))showReplayOverlay(false,state.score);
    }
    gameAnimId=requestAnimationFrame(update);
  }
  update();
  return()=>{
    if(gameAnimId){cancelAnimationFrame(gameAnimId);gameAnimId=null;}
    const ov=document.getElementById('replayOverlay');
    if(ov)ov.parentNode.removeChild(ov);
    canvas.removeEventListener('mousedown',onMouseDown);
    canvas.removeEventListener('mousemove',onMouseMove);
    canvas.removeEventListener('mouseup',onMouseUp);
    canvas.removeEventListener('touchstart',onTouchStart);
    canvas.removeEventListener('touchmove',onTouchMove);
    canvas.removeEventListener('touchend',onTouchEnd);
  };
}

/* ── level generators ── */
function generateTargets(level,cw,groundY){
  const count=Math.min(2+level,6),targets=[];
  const startX=cw*0.2,endX=cw*0.9,spacing=(endX-startX)/(count+1);
  for(let i=0;i<count;i++){
    const x=startX+spacing*(i+1),baseH=25+level*6;
    targets.push({id:`T${i+1}`,x:Math.round(x),y:Math.round(Math.max(groundY-baseH-Math.random()*25,60)),w:32,h:28});
  }
  return targets;
}
function generateWalls(level,cw,groundY){
  const count=Math.min(Math.floor(level/2)+1,4),walls=[];
  const startX=cw*0.15,endX=cw*0.75,spacing=(endX-startX)/(count+2);
  for(let i=0;i<count;i++){
    const x=startX+spacing*(i+1),h=50+level*10+Math.random()*20;
    walls.push({x:Math.round(x),y:Math.round(groundY-h),w:16,h:Math.round(h)});
  }
  return walls;
}

/* ── load session ── */
let gameCleanup=null;
function loadSession(s){
  session=s;trajectories=[];bounceTrajs=[];collisionPts=[];
  currentPlotIdx=-1;
  isAnim=false;if(animH)cancelAnimationFrame(animH);
  af=0;progressBar.style.width='0%';runBtn.disabled=false;
  tooltip.style.display='none';
  if(gameCleanup){gameCleanup();gameCleanup=null;}
  canvas.onmousedown=canvas.onmousemove=canvas.onmouseup=canvas.onmouseleave=null;
  teardown3D();

  const threeContainer=document.getElementById('threejsContainer');
  const hint3d=document.getElementById('hint3d');

  /* ── game mode always uses the 2D canvas ── */
  if(s.mode==='game'){
    document.getElementById('toggleDim').style.display='none';
    document.getElementById('legendTitle').textContent='Controls';
    document.getElementById('plotSelector').style.display='none';
    canvas.style.display='';
    threeContainer.style.display='none';
    hint3d.style.display='none';
    canvas.width=canvas.clientWidth;canvas.height=canvas.clientHeight;
    updateSidebar(s);
    gameCleanup=renderGame(ctx,canvas,s);
    document.getElementById('canvasLabel').textContent='GAME MODE';
    return;
  }

  /* ── fork and simulate both support 3D ── */
  document.getElementById('toggleDim').style.display='';
  document.getElementById('legendTitle').textContent='Trajectories';

  /* ── 3D mode (simulate and fork) ── */
  if(is3D){
    canvas.style.display='none';
    threeContainer.style.display='block';
    hint3d.style.display='block';
    document.getElementById('canvasLabel').textContent='3D TRAJECTORY VIEW';
    updateSidebar(s);
    load3D(s);
    return;
  }

  /* ── 2D mode (original path, unchanged) ── */
  canvas.style.display='';
  threeContainer.style.display='none';
  hint3d.style.display='none';
  document.getElementById('canvasLabel').textContent='TRAJECTORY PLOT';
  const g=s.gravity,ar=s.air_resistance||false;
  (s.projectiles||[]).forEach(p=>{
    const pg=p.branchGravity!=null?p.branchGravity:g;
    const col=p.branchColor||colorFor(p.id);
    const pts=buildTraj(p,pg,ar,s.air_density||1.225,s.wind_x||0,s.wind_y||0);
    const delay=delayToFrames(p.launch_delay||0);
    trajectories.push({id:p.id,color:col,points:pts,branchLabel:p.branchLabel||null,launchDelay:delay});
  });
  (s.bounces||[]).forEach(b=>{
    const bg=b.branchGravity!=null?b.branchGravity:g;
    /* chain: arc 0 starts after parent trajectory lands, each subsequent arc after the previous */
    const parentTr=trajectories.find(t=>t.id===b.p);
    let arcDelay=parentTr?((parentTr.launchDelay||0)+parentTr.points.length):0;
    (b.arcs||[]).forEach(arc=>{
      const pts=buildBounceTraj(arc,bg);
      bounceTrajs.push({id:b.p+'_b',color:colorFor(b.p),points:pts,dashed:false,launchDelay:arcDelay});
      arcDelay+=pts.length;
    });
  });
  (s.collisions||[]).forEach(c=>collisionPts.push({x:c.x,y:c.y,label:c.p1+'x'+c.p2}));
  totF=Math.max(...[...trajectories,...bounceTrajs].map(t=>(t.launchDelay||0)+t.points.length),1);
  canvas.onmousemove=handleSimHover;
  canvas.onmouseleave=()=>{tooltip.style.display='none';chH.style.opacity='0';chV.style.opacity='0';};
  updateSidebar(s);
  setupPlotSelector(s);
  resize();
}

/* ── animation (RUN button) ── */
function startAnimation(){
  if(!session||session.mode==='game')return;
  if(is3D){start3DAnim();return;}
  af=0;isAnim=true;runBtn.disabled=true;progressBar.style.width='0%';
  (function loop(){
    renderSim();progressBar.style.width=Math.min(af/totF*100,100)+'%';
    af+=3;
    if(af<totF+10)animH=requestAnimationFrame(loop);
    else{isAnim=false;af=totF+10;renderSim();progressBar.style.width='100%';runBtn.disabled=false;}
  })();
}

function resize(){
  canvas.width=canvas.clientWidth;canvas.height=canvas.clientHeight;
  if(session&&session.mode==='game')return;
  if(is3D){
    const c=document.getElementById('threejsContainer');
    if(threeState&&c.clientWidth>0){
      threeState.resize(c.clientWidth,c.clientHeight);
    }
    return;
  }
  /* dispatch to the currently selected plot or trajectory view */
  if(currentPlotIdx>=0&&session){
    const plots=(session.plots||[]).concat((session.annotations||[]).filter(a=>a.type==='plot'));
    if(plots[currentPlotIdx]){renderPlot(plots[currentPlotIdx]);return;}
  }
  renderSim();
}

/* ── hover (sim only) ── */
function handleSimHover(e){
  if(isAnim)return;
  const rect=canvas.getBoundingClientRect();
  const mx=e.clientX-rect.left,my=e.clientY-rect.top;
  chH.style.opacity='1';chV.style.opacity='1';
  chH.style.top=my+'px';chV.style.left=mx+'px';
  let best=null,bestD=18;
  [...trajectories,...bounceTrajs].forEach(tr=>
    tr.points.forEach(p=>{
      const{cx,cy}=toC(p.x,p.y);const d=Math.hypot(cx-mx,cy-my);
      if(d<bestD){bestD=d;best={...p,id:tr.id,color:tr.color};}
    })
  );
  if(best){
    tooltip.style.display='block';
    tooltip.style.left=(e.clientX+16)+'px';tooltip.style.top=(e.clientY-50)+'px';
    document.getElementById('tt-id').textContent=best.id;
    document.getElementById('tt-id').style.color=best.color||'var(--glow)';
    document.getElementById('tt-x').textContent=best.x.toFixed(3);
    document.getElementById('tt-y').textContent=best.y.toFixed(3);
    document.getElementById('tt-t').textContent=best.t.toFixed(3);
  }else{tooltip.style.display='none';}
}

/* ── sidebar ── */
function updateSidebar(s){
  const leg=document.getElementById('legend');
  if(s.mode==='game'){
    const pl=getPlanet(s.planet||'earth');
    leg.innerHTML=`<div class="legend-item">
      <div class="legend-dot" style="background:${pl.accent};box-shadow:0 0 4px ${pl.accent}"></div>
      <span>${(s.planet||'planet').toUpperCase()}</span></div>
      <div style="font-size:.62rem;color:var(--dim);margin-top:8px;line-height:1.9;font-family:var(--font-ui)">
        🎯 Drag to aim &nbsp; ⚡ Hold for power<br>▶ Click &amp; release to fire</div>`;
  }else{
    const projs=s.projectiles||[];
    const ids=[...new Set(projs.map(p=>p.id))];
    leg.innerHTML=ids.map(id=>{
      const col=projs.find(p=>p.id===id)?.branchColor||colorFor(id);
      const bl=projs.find(p=>p.id===id)?.branchLabel;
      return`<div class="legend-item">
        <div class="legend-dot" style="background:${col};box-shadow:0 0 4px ${col}"></div>
        <span>${id}${bl?' ('+bl+')':''}</span></div>`;
    }).join('');
    const shapes=[...new Set(projs.map(p=>getShape(p)))];
    const sd={sphere:'● sphere (Cd < 0.6)',cuboid:'▬ cuboid (0.6 \u2264 Cd < 0.9)',cube:'■ cube (Cd \u2265 0.9)'};
    if(shapes.length)leg.innerHTML+=
      `<div style="margin-top:8px;border-top:1px solid var(--border);padding-top:6px">`+
      shapes.map(sh=>`<div style="font-size:.6rem;color:var(--dim);padding:2px 0;font-family:var(--font-ui)">${sd[sh]||sh}</div>`).join('')+'</div>';
  }
  const ev=document.getElementById('envContent');
  if(s.mode==='game'){
    const pl=getPlanet(s.planet||'earth');
    ev.innerHTML=`
      <div class="env-row"><span class="env-key">PLANET</span><span class="env-val" style="color:${pl.accent}">${pl.emoji} ${(s.planet||'?').toUpperCase()}</span></div>
      <div class="env-row"><span class="env-key">GRAVITY</span><span class="env-val">${parseFloat(s.gravity).toFixed(2)} m/s\u00B2</span></div>
      <div class="env-row"><span class="env-key">LEVEL</span><span class="env-val">${s.level}</span></div>
      <div class="env-row"><span class="env-key">LIVES</span><span class="env-val" style="color:#ff4d6d">${'\u2764 '.repeat(Math.min(parseInt(s.lives)||0,5)).trim()}</span></div>`;
  }else{
    const ar=s.air_resistance;
    const wz=parseFloat(s.wind_z||0);
    ev.innerHTML=`
      <div class="env-row"><span class="env-key">GRAVITY</span><span class="env-val">${parseFloat(s.gravity).toFixed(2)} m/s\u00B2</span></div>
      <div class="env-row"><span class="env-key">AIR DRAG</span><span class="env-val" style="color:${ar?'#ff4d6d':'#39ff8f'}">${ar?'ON':'OFF'}</span></div>
      ${ar?`<div class="env-row"><span class="env-key">AIR \u03C1</span><span class="env-val">${parseFloat(s.air_density).toFixed(4)}</span></div>
      <div class="env-row"><span class="env-key">WIND X</span><span class="env-val">${parseFloat(s.wind_x).toFixed(2)} m/s</span></div>
      <div class="env-row"><span class="env-key">WIND Y</span><span class="env-val">${parseFloat(s.wind_y).toFixed(2)} m/s</span></div>
      <div class="env-row"><span class="env-key">WIND Z</span><span class="env-val">${wz.toFixed(2)} m/s</span></div>`:''}
      ${is3D?`<div class="env-row"><span class="env-key">RENDER</span><span class="env-val" style="color:var(--purple)">3D / THREE.JS</span></div>`:''}`;
  }
  const mc=document.getElementById('metricsContent');
  const qs=s.queries||[];
  if(!qs.length){mc.innerHTML='<div style="color:var(--dim);font-size:.72rem;font-family:var(--font-ui)">No queries in this block.</div>';return;}
  mc.innerHTML=qs.map(q=>{
    const v=String(q.value);
    const cls=v.includes('PASS')?'pass':v.includes('FAIL')?'fail':(v.includes('YES')||v.includes('NO'))?'col':'';
    const vcls=v.includes('FAIL')?'fail':'';
    return`<div class="q-card ${cls}">
      <div class="q-label">${q.label}</div>
      <div class="q-value ${vcls}">${typeof q.value==='number'?q.value.toFixed(4):q.value}<span class="q-unit">${q.unit||''}</span></div>
      ${q.note?`<div class="q-note">${q.note}</div>`:''}</div>`;
  }).join('');
}

/* ── tabs + init ── */
function init(){
  const tc=document.getElementById('tabs');
  DATA.forEach((s,i)=>{
    const isFork=s.mode==='fork',isGame=s.mode==='game';
    const btn=document.createElement('button');
    btn.className='tab-btn'+(isFork?' fork-tab':isGame?' game-tab':'')+(i===0?' active':'');
    btn.textContent=s.label||(isGame?'Game '+(i+1):isFork?'Fork '+(i+1):'Sim '+(i+1));
    btn.title=s.label||'';
    btn.onclick=()=>{
      document.querySelectorAll('.tab-btn').forEach(b=>b.classList.remove('active'));
      btn.classList.add('active');
      tooltip.style.display='none';chH.style.opacity='0';chV.style.opacity='0';
      loadSession(s);
    };
    tc.appendChild(btn);
  });
  window.addEventListener('resize',resize);
  if(DATA.length>0)loadSession(DATA[0]);
}
window.onload=init;
</script>
</body>
</html>
|html}

(* ══════════════════════════════════════════════════════════════════
   ENTRY POINT
   ══════════════════════════════════════════════════════════════════ *)
let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Usage: %s <input.px> [output.html]\n" Sys.argv.(0);
    exit 1
  end;

  let input_file = Sys.argv.(1) in

  let html_out =
    if Array.length Sys.argv >= 3 then Sys.argv.(2)
    else Filename.remove_extension input_file ^ ".html"
  in

  (* images live in  <same-dir-as-input>/assets/ *)
  let asset_dir = Filename.concat (Filename.dirname input_file) "assets" in
  let img path mime = data_uri mime (Filename.concat asset_dir path) in

  (* pipeline *)
  let src =
    try My_utils.read_file input_file
    with Sys_error msg ->
      Printf.eprintf "Cannot open: %s\n" msg;
      exit 1
  in
  let tokens =
    try Tokenizer.tokenize (explode src)
    with Failure msg ->
      Printf.eprintf "Lex error: %s\n" msg;
      exit 1
  in
  let program =
    try Parser.parse tokens
    with Failure msg ->
      Printf.eprintf "Parse error: %s\n" msg;
      exit 1
  in
  (try Checker.check program
   with Failure msg ->
     Printf.eprintf "Semantic error: %s\n" msg;
     exit 1);

  (* original evaluator — stdout unchanged *)
  (try Eval.eval_program program
   with Failure msg ->
     Printf.eprintf "Runtime error: %s\n" msg;
     exit 1);

  (* JSON *)
  let json =
    try Projx.Json_emit.emit_json program
    with Failure msg ->
      Printf.eprintf "JSON error: %s\n" msg;
      exit 1
  in

  (* base64 images *)
  let img_mars = img "mars.png" "image/png" in
  let img_jupiter = img "jupiter.png" "image/png" in
  let img_saturn = img "saturn.png" "image/png" in
  let img_moon = img "planet1.jpeg" "image/jpeg" in
  let img_earth = img "bgearth.jpeg" "image/jpeg" in

  let html =
    html_template
    |> replace_all "__PROJX_DATA__" json
    |> replace_all "__IMG_MARS__" img_mars
    |> replace_all "__IMG_JUPITER__" img_jupiter
    |> replace_all "__IMG_SATURN__" img_saturn
    |> replace_all "__IMG_MOON__" img_moon
    |> replace_all "__IMG_EARTH__" img_earth
  in

  (try
     Out_channel.with_open_text html_out (fun oc ->
         Out_channel.output_string oc html)
   with Sys_error msg ->
     Printf.eprintf "Write error: %s\n" msg;
     exit 1);

  Printf.printf "\n[ProjX] Written: %s\n" html_out;
  Printf.printf "[ProjX] Open in any browser — no server needed.\n"