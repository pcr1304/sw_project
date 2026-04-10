// ── ProjX Canvas Renderer v3 ──
// Auto-zoom simulation, floating planet sprites in game, proper range.

const COLORS = ["#38d9f5", "#a78bfa", "#f5a623", "#3de87a", "#f55a6e"];

// ═══════════════════════════════════
//  Image preloader
// ═══════════════════════════════════

const planetImages = {};
const planetImgSources = {
    moon: "planet1.jpeg",
    mars: "mars.png",
    jupiter: "jupiter.png",
    saturn: "saturn.png"
};

(function preloadImages() {
    for (const [planet, src] of Object.entries(planetImgSources)) {
        const img = new Image();
        img.src = src;
        planetImages[planet] = img;
    }
})();


// ═══════════════════════════════════
//  SIMULATION MODE  (auto-zoom)
// ═══════════════════════════════════

let simAnimId = null;
let simTime = 0;

function computeSimBounds(projectiles, gravity) {
    let maxX = 0, maxY = 0;
    projectiles.forEach(p => {
        const theta = p.angle * (Math.PI / 180);
        const x0 = p.launch_from ? p.launch_from[0] : 0;
        const y0 = p.launch_from ? p.launch_from[1] : 0;
        const vY = p.speed * Math.sin(theta);
        const vX = p.speed * Math.cos(theta);
        const timeToGround = (vY + Math.sqrt(vY * vY + 2 * gravity * y0)) / gravity;
        const r = x0 + vX * timeToGround;
        const mh = (vY * vY) / (2 * gravity) + y0;
        maxX = Math.max(maxX, r);
        maxY = Math.max(maxY, mh);
    });
    return { maxX: Math.max(maxX, 10), maxY: Math.max(maxY, 5) };
}

function renderSimulation(ctx, canvas, data, onFinish) {
    if (simAnimId) { cancelAnimationFrame(simAnimId); simAnimId = null; }
    simTime = 0;

    const g = data.gravity || 9.8;
    const bounds = computeSimBounds(data.projectiles || [], g);
    const marginPx = 80;
    const usableW = canvas.width - marginPx - 40;
    const usableH = canvas.height - marginPx - 20;
    const scaleX = bounds.maxX > 0 ? usableW / bounds.maxX : 5;
    const scaleY = bounds.maxY > 0 ? usableH / bounds.maxY : 5;
    const scale = Math.max(Math.min(scaleX, scaleY, 12), 0.3);

    const groundY = canvas.height - 40;
    const originX = 50;

    function niceStep(maxVal) {
        if (maxVal <= 0) return 10;
        const raw = maxVal / 8;
        const mag = Math.pow(10, Math.floor(Math.log10(raw)));
        const norm = raw / mag;
        if (norm < 1.5) return mag;
        if (norm < 3.5) return 2 * mag;
        if (norm < 7.5) return 5 * mag;
        return 10 * mag;
    }

    const xStep = niceStep(bounds.maxX);
    const yStep = niceStep(bounds.maxY);

    function animate() {
        ctx.fillStyle = "#13141a";
        ctx.fillRect(0, 0, canvas.width, canvas.height);
        ctx.fillStyle = "#0f1015";
        ctx.fillRect(0, groundY, canvas.width, canvas.height - groundY);

        // grid
        ctx.strokeStyle = "rgba(42,45,58,0.5)";
        ctx.lineWidth = 0.5;
        for (let m = xStep; m <= bounds.maxX * 1.2; m += xStep) {
            const px = originX + m * scale;
            if (px > canvas.width) break;
            ctx.beginPath(); ctx.moveTo(px, 20); ctx.lineTo(px, groundY); ctx.stroke();
        }
        for (let m = yStep; m <= bounds.maxY * 1.3; m += yStep) {
            const py = groundY - m * scale;
            if (py < 20) break;
            ctx.beginPath(); ctx.moveTo(originX, py); ctx.lineTo(canvas.width - 10, py); ctx.stroke();
        }

        // axes
        ctx.beginPath(); ctx.moveTo(originX, groundY); ctx.lineTo(canvas.width - 10, groundY);
        ctx.strokeStyle = "#2a2d3a"; ctx.lineWidth = 1; ctx.stroke();

        ctx.fillStyle = "#3a3d4a";
        ctx.font = "10px 'JetBrains Mono', monospace";
        for (let m = 0; m <= bounds.maxX * 1.2; m += xStep) {
            const px = originX + m * scale;
            if (px > canvas.width - 20) break;
            ctx.fillText(fmtLabel(m), px - 8, groundY + 14);
        }
        for (let m = yStep; m <= bounds.maxY * 1.3; m += yStep) {
            const py = groundY - m * scale;
            if (py < 25) break;
            ctx.fillText(fmtLabel(m), 4, py + 4);
        }

        let allDone = true;
        if (data.projectiles) {
            data.projectiles.forEach((proj, i) => {
                ctx.strokeStyle = COLORS[i % COLORS.length];
                if (data.launched) {
                    const done = drawLiveArc(ctx, proj, g, groundY, scale, simTime, originX);
                    if (!done) allDone = false;
                } else {
                    drawLiveArc(ctx, proj, g, groundY, scale, 0, originX);
                }
            });
        }

        if (!data.launched) {
            drawAnnotations(ctx, canvas, data, groundY, scale, originX);
        } else if (allDone) {
            drawAnnotations(ctx, canvas, data, groundY, scale, originX);
            if (onFinish) onFinish();
        } else {
            simTime += 0.05;
            simAnimId = requestAnimationFrame(animate);
        }
    }
    animate();
}

function fmtLabel(m) {
    if (m >= 1000) return (m / 1000).toFixed(1) + "k";
    if (m === Math.floor(m)) return m.toString();
    return m.toFixed(1);
}

function stopSimulation() {
    if (simAnimId) { cancelAnimationFrame(simAnimId); simAnimId = null; }
}

function drawLiveArc(ctx, p, g, groundY, scale, maxT, originX) {
    const angle = p.angle || 45;
    const speed = p.speed || 30;
    const lx = p.launch_from ? p.launch_from[0] : 0;
    const ly = p.launch_from ? p.launch_from[1] : 0;
    const theta = angle * (Math.PI / 180);
    const startX = originX + lx * scale;
    const startY = groundY - ly * scale;

    ctx.beginPath();
    ctx.arc(startX, startY, 3, 0, Math.PI * 2);
    ctx.fillStyle = ctx.strokeStyle;
    ctx.fill();

    ctx.beginPath();
    ctx.moveTo(startX, startY);

    const vY = speed * Math.sin(theta);
    const vX = speed * Math.cos(theta);
    const timeToGround = (vY + Math.sqrt(vY * vY + 2 * g * ly)) / g;

    let t = 0, curX = startX, curY = 0, hit = false;
    while (t <= maxT) {
        if (t > 0 && t >= timeToGround) {
            curX = startX + (vX * timeToGround) * scale;
            curY = startY - groundY;
            hit = true;
            ctx.lineTo(curX, groundY);
            break;
        }
        const px = vX * t;
        const py = vY * t - 0.5 * g * t * t;
        curX = startX + px * scale;
        curY = py * scale;
        ctx.lineTo(curX, startY - curY);
        t += 0.05;
    }

    ctx.lineWidth = 2.2; ctx.lineCap = "round"; ctx.stroke();

    ctx.beginPath();
    ctx.arc(curX, startY - curY, 5, 0, Math.PI * 2);
    ctx.fillStyle = ctx.strokeStyle;
    ctx.shadowBlur = 8; ctx.shadowColor = ctx.strokeStyle;
    ctx.fill(); ctx.shadowBlur = 0;

    return hit;
}

function drawAnnotations(ctx, canvas, data, groundY, scale, originX) {
    if (!data.annotations) return;
    data.annotations.forEach(anno => {
        const p = data.projectiles.find(proj => proj.id === anno.p);
        if (!p) return;
        const theta = p.angle * (Math.PI / 180);
        const lx = p.launch_from ? p.launch_from[0] : 0;
        const ly = p.launch_from ? p.launch_from[1] : 0;
        ctx.font = "11px 'JetBrains Mono', monospace";

        if (anno.type === "max_height") {
            const timeToPeak = (p.speed * Math.sin(theta)) / data.gravity;
            const peakX = originX + lx * scale + p.speed * Math.cos(theta) * timeToPeak * scale;
            const peakY = groundY - anno.value * scale;
            const startY = groundY - ly * scale;
            ctx.beginPath(); ctx.setLineDash([4, 4]);
            ctx.moveTo(peakX, groundY); ctx.lineTo(peakX, peakY);
            if (ly > 0) {
                ctx.moveTo(peakX - 5, startY); ctx.lineTo(peakX + 5, startY);
            }
            ctx.strokeStyle = "#a78bfa"; ctx.lineWidth = 1; ctx.stroke(); ctx.setLineDash([]);
            const hFromY = anno.value - ly;
            const lbl = ly > 0 ? `max: ${anno.value.toFixed(1)}m (from y0: ${hFromY.toFixed(1)}m)` : `max_h: ${anno.value.toFixed(2)}m`;
            const tw = ctx.measureText(lbl).width + 16;
            ctx.fillStyle = "rgba(19,20,26,0.9)";
            ctx.fillRect(peakX + 6, peakY - 12, tw, 22);
            ctx.strokeRect(peakX + 6, peakY - 12, tw, 22);
            ctx.fillStyle = "#a78bfa";
            ctx.fillText(lbl, peakX + 14, peakY + 3);
        }
        if (anno.type === "range") {
            const startX = originX + lx * scale;
            const endX = originX + anno.value * scale;
            ctx.beginPath(); ctx.setLineDash([3, 3]);
            ctx.moveTo(startX, groundY + 18); ctx.lineTo(endX, groundY + 18);
            ctx.strokeStyle = "#3de87a"; ctx.lineWidth = 1; ctx.stroke(); ctx.setLineDash([]);
            ctx.beginPath();
            ctx.moveTo(startX, groundY + 14); ctx.lineTo(startX, groundY + 22);
            ctx.moveTo(endX, groundY + 14); ctx.lineTo(endX, groundY + 22); ctx.stroke();
            const rLbl = `${anno.value.toFixed(2)}m`;
            ctx.fillStyle = "#3de87a";
            ctx.fillText(rLbl, (startX + endX) / 2 - ctx.measureText(rLbl).width / 2, groundY + 32);
        }
    });
}


// ═══════════════════════════════════
//  GAME MODE — enhanced visuals
// ═══════════════════════════════════

let gameAnimId = null;

const planetThemes = {
    earth: { skyTop: "#020818", skyMid: "#0a1628", skyBot: "#162040", nebula: "rgba(56,130,245,0.04)", ground: "#1a2030", horizon: "#38d9f5" },
    moon: { skyTop: "#0a0a0a", skyMid: "#111118", skyBot: "#1a1a22", nebula: "rgba(180,180,220,0.03)", ground: "#1c1c20", horizon: "#bbbbcc" },
    mars: { skyTop: "#120808", skyMid: "#1e0e0a", skyBot: "#2a1510", nebula: "rgba(220,80,40,0.04)", ground: "#1e1210", horizon: "#e87040" },
    jupiter: { skyTop: "#0c0a04", skyMid: "#1a1508", skyBot: "#28200c", nebula: "rgba(245,180,60,0.04)", ground: "#1e1a10", horizon: "#f5b040" },
    saturn: { skyTop: "#0a0808", skyMid: "#161010", skyBot: "#221a14", nebula: "rgba(200,150,100,0.04)", ground: "#1a1612", horizon: "#c8a060" }
};

function renderGame(ctx, canvas, data) {
    if (gameAnimId) { cancelAnimationFrame(gameAnimId); gameAnimId = null; }

    const groundY = canvas.height - 40;
    const g = data.gravity || 9.8;
    const launchSpeed = data.launchSpeed || 30;

    function getGameScale() {
        // fixed game scale
        return Math.min(canvas.width / 40, 25);
    }

    const state = { lives: data.lives || 3, score: 0, activeBullets: [] };

    let aimAngle = Math.PI / 4;
    let aimDist = 0;
    let aiming = false;
    const launcherX = 50, launcherY = groundY;

    const particles = [];
    const scorePopups = [];

    // stars
    const stars = [];
    for (let i = 0; i < 120; i++) {
        const hue = Math.random() > 0.85 ? (180 + Math.random() * 60) : 0;
        stars.push({
            x: Math.random() * canvas.width, y: Math.random() * groundY * 0.85,
            r: Math.random() * 1.8 + 0.2, o: Math.random() * 0.5 + 0.1,
            twinkle: 0.008 + Math.random() * 0.04, hue
        });
    }

    const targets = data.targets && data.targets.length > 0 ? data.targets : generateTargets(data.level || 1, canvas.width, groundY);
    const walls = data.walls && data.walls.length > 0 ? data.walls : generateWalls(data.level || 1, canvas.width, groundY);
    const hitTargets = new Set();
    let frameCount = 0;

    const planetName = data.planet || "earth";
    const theme = planetThemes[planetName] || planetThemes.earth;
    const floatingPlanet = planetImages[planetName] || null;
    const planetSize = 110;
    const planetCX = canvas.width * 0.68;
    const planetBY = groundY * 0.22;

    let mouseStartX = 0, mouseStartY = 0;

    function getAngleFromMouse(e) {
        const r = canvas.getBoundingClientRect();
        const mx = (e.clientX - r.left) * (canvas.width / r.width);
        const my = (e.clientY - r.top) * (canvas.height / r.height);
        return Math.max(0.08, Math.min(Math.atan2(launcherY - my, mx - launcherX), Math.PI / 2 - 0.05));
    }
    function getDragDist(e) {
        const r = canvas.getBoundingClientRect();
        const mx = (e.clientX - r.left) * (canvas.width / r.width);
        const my = (e.clientY - r.top) * (canvas.height / r.height);
        return Math.sqrt((mx - mouseStartX) ** 2 + (my - mouseStartY) ** 2);
    }

    let curLaunchSpeed = launchSpeed;

    const onMouseDown = (e) => {
        if (state.lives <= 0) return;
        const r = canvas.getBoundingClientRect();
        mouseStartX = (e.clientX - r.left) * (canvas.width / r.width);
        mouseStartY = (e.clientY - r.top) * (canvas.height / r.height);
        aiming = true; aimAngle = getAngleFromMouse(e); aimDist = 0;
        curLaunchSpeed = Math.max(10, Math.min(aimDist / 3.5, 60));
    };
    const onMouseMove = (e) => {
        if (!aiming) return;
        aimAngle = getAngleFromMouse(e);
        aimDist = getDragDist(e);
        curLaunchSpeed = Math.max(10, Math.min(aimDist / 3.5, 60));
    };
    const onMouseUp = (e) => {
        if (!aiming) return; aiming = false; if (state.lives <= 0) return;
        aimAngle = getAngleFromMouse(e);
        curLaunchSpeed = Math.max(10, Math.min(aimDist / 3.5, 60));
        state.activeBullets.push({
            x: launcherX, y: launcherY,
            vx: curLaunchSpeed * Math.cos(aimAngle), vy: curLaunchSpeed * Math.sin(aimAngle), trail: []
        });
        state.lives--;
    };
    const onTouchStart = (e) => { e.preventDefault(); onMouseDown(e.touches[0]); };
    const onTouchMove = (e) => { e.preventDefault(); onMouseMove(e.touches[0]); };
    const onTouchEnd = (e) => { e.preventDefault(); onMouseUp(e.changedTouches[0]); };

    canvas.addEventListener("mousedown", onMouseDown);
    canvas.addEventListener("mousemove", onMouseMove);
    canvas.addEventListener("mouseup", onMouseUp);
    canvas.addEventListener("touchstart", onTouchStart, { passive: false });
    canvas.addEventListener("touchmove", onTouchMove, { passive: false });
    canvas.addEventListener("touchend", onTouchEnd, { passive: false });

    function spawnExplosion(x, y) {
        for (let i = 0; i < 20; i++) {
            const a = Math.random() * Math.PI * 2, s = 1 + Math.random() * 3.5;
            particles.push({
                x, y, vx: Math.cos(a) * s, vy: Math.sin(a) * s - 1.5,
                life: 40 + Math.random() * 20, maxLife: 60,
                color: Math.random() > 0.5 ? "#3de87a" : "#f5a623", r: 1.5 + Math.random() * 2.5
            });
        }
    }

    // ── draw ──

    function drawEnv() {
        const sky = ctx.createLinearGradient(0, 0, 0, groundY);
        sky.addColorStop(0, theme.skyTop); sky.addColorStop(0.5, theme.skyMid); sky.addColorStop(1, theme.skyBot);
        ctx.fillStyle = sky; ctx.fillRect(0, 0, canvas.width, groundY);

        // nebula
        const nt = frameCount * 0.001;
        for (let i = 0; i < 3; i++) {
            const nx = canvas.width * (0.2 + i * 0.3) + Math.sin(nt + i) * 80;
            const ny = groundY * (0.2 + i * 0.15) + Math.cos(nt * 0.7 + i) * 40;
            const nr = 120 + i * 40;
            const ng = ctx.createRadialGradient(nx, ny, 0, nx, ny, nr);
            ng.addColorStop(0, theme.nebula); ng.addColorStop(1, "rgba(0,0,0,0)");
            ctx.fillStyle = ng; ctx.fillRect(nx - nr, ny - nr, nr * 2, nr * 2);
        }

        // stars
        stars.forEach(s => {
            const f = Math.sin(frameCount * s.twinkle) * 0.25 + 0.75;
            ctx.fillStyle = s.hue ? `hsla(${s.hue},60%,75%,${s.o * f})` : `rgba(255,255,255,${s.o * f})`;
            ctx.beginPath(); ctx.arc(s.x, s.y, s.r, 0, Math.PI * 2); ctx.fill();
        });

        // floating planet — circular clip to hide any opaque backgrounds
        if (floatingPlanet && floatingPlanet.complete && floatingPlanet.naturalWidth > 0) {
            const ox = planetCX + Math.sin(frameCount * 0.002) * 50;
            const oy = planetBY + Math.sin(frameCount * 0.006) * 10;
            const radius = planetSize / 2;

            // glow behind planet
            const gg = ctx.createRadialGradient(ox, oy, radius * 0.4, ox, oy, radius * 1.8);
            gg.addColorStop(0, theme.nebula.replace("0.04", "0.15"));
            gg.addColorStop(1, "rgba(0,0,0,0)");
            ctx.fillStyle = gg;
            ctx.beginPath(); ctx.arc(ox, oy, radius * 1.8, 0, Math.PI * 2); ctx.fill();

            // circular clip mask — hides jpeg backgrounds
            ctx.save();
            ctx.beginPath();
            ctx.arc(ox, oy, radius, 0, Math.PI * 2);
            ctx.clip();

            ctx.globalAlpha = 0.8;
            const aspect = floatingPlanet.naturalWidth / floatingPlanet.naturalHeight;
            const drawW = Math.max(planetSize, planetSize * aspect);
            const drawH = Math.max(planetSize, planetSize / aspect);
            ctx.drawImage(floatingPlanet, ox - drawW / 2, oy - drawH / 2, drawW, drawH);
            ctx.globalAlpha = 1;

            ctx.restore();

            // subtle orbital ring
            ctx.beginPath();
            ctx.arc(ox, oy, radius + 2, 0, Math.PI * 2);
            ctx.strokeStyle = theme.horizon + "30";
            ctx.lineWidth = 1;
            ctx.stroke();
        } else {
            // procedural planet globe (Earth fallback)
            const ox = planetCX + Math.sin(frameCount * 0.002) * 50;
            const oy = planetBY + Math.sin(frameCount * 0.006) * 10;
            const radius = planetSize / 2;

            // atmosphere glow
            const atm = ctx.createRadialGradient(ox, oy, radius * 0.6, ox, oy, radius * 1.5);
            atm.addColorStop(0, "rgba(56,180,245,0.1)");
            atm.addColorStop(1, "rgba(0,0,0,0)");
            ctx.fillStyle = atm;
            ctx.beginPath(); ctx.arc(ox, oy, radius * 1.5, 0, Math.PI * 2); ctx.fill();

            // globe body
            const globe = ctx.createRadialGradient(ox - radius * 0.3, oy - radius * 0.3, 0, ox, oy, radius);
            globe.addColorStop(0, "#4488cc");
            globe.addColorStop(0.4, "#2266aa");
            globe.addColorStop(0.7, "#1a4488");
            globe.addColorStop(1, "#0a2244");
            ctx.fillStyle = globe;
            ctx.beginPath(); ctx.arc(ox, oy, radius, 0, Math.PI * 2); ctx.fill();

            // "continents" — simple green patches
            ctx.globalAlpha = 0.3;
            const cTime = frameCount * 0.001;
            for (let c = 0; c < 4; c++) {
                const cx = ox + Math.cos(cTime + c * 1.6) * radius * 0.45;
                const cy = oy + Math.sin(cTime * 0.7 + c * 2) * radius * 0.35;
                const cr = radius * (0.12 + c * 0.04);
                ctx.fillStyle = "#2a8844";
                ctx.beginPath(); ctx.arc(cx, cy, cr, 0, Math.PI * 2); ctx.fill();
            }
            ctx.globalAlpha = 1;

            // sheen highlight
            const shine = ctx.createRadialGradient(ox - radius * 0.35, oy - radius * 0.35, 0, ox, oy, radius);
            shine.addColorStop(0, "rgba(255,255,255,0.15)");
            shine.addColorStop(0.5, "rgba(255,255,255,0)");
            ctx.fillStyle = shine;
            ctx.beginPath(); ctx.arc(ox, oy, radius, 0, Math.PI * 2); ctx.fill();

            // orbital ring
            ctx.beginPath();
            ctx.arc(ox, oy, radius + 2, 0, Math.PI * 2);
            ctx.strokeStyle = theme.horizon + "30";
            ctx.lineWidth = 1;
            ctx.stroke();
        }

        // ground — layered per planet theme
        const gGrad = ctx.createLinearGradient(0, groundY, 0, canvas.height);
        gGrad.addColorStop(0, theme.ground); gGrad.addColorStop(1, "#080810");
        ctx.fillStyle = gGrad; ctx.fillRect(0, groundY, canvas.width, canvas.height - groundY);

        // surface ridges / texture lines (planet-specific feel)
        ctx.strokeStyle = theme.horizon + "12";
        ctx.lineWidth = 0.5;
        for (let ry = groundY + 6; ry < canvas.height; ry += 8) {
            ctx.beginPath();
            ctx.moveTo(0, ry);
            for (let rx = 0; rx < canvas.width; rx += 40) {
                ctx.lineTo(rx + 20, ry + Math.sin(rx * 0.02 + ry * 0.1) * 1.5);
            }
            ctx.stroke();
        }

        // horizon glow
        ctx.beginPath(); ctx.moveTo(0, groundY); ctx.lineTo(canvas.width, groundY);
        ctx.strokeStyle = theme.horizon; ctx.lineWidth = 2;
        ctx.shadowBlur = 12; ctx.shadowColor = theme.horizon; ctx.stroke(); ctx.shadowBlur = 0;
    }

    function drawLauncher() {
        ctx.save(); ctx.translate(launcherX, launcherY); ctx.rotate(-aimAngle);
        ctx.fillStyle = "rgba(0,0,0,0.3)"; ctx.fillRect(2, -5, 32, 12);
        const bg = ctx.createLinearGradient(0, -7, 0, 7);
        bg.addColorStop(0, "#4ae0fa"); bg.addColorStop(1, "#1a8aaa");
        ctx.fillStyle = bg; ctx.strokeStyle = theme.horizon; ctx.lineWidth = 1.5;
        ctx.beginPath(); ctx.roundRect(0, -7, 32, 14, 3); ctx.fill(); ctx.stroke();
        ctx.fillStyle = aiming ? "#f5a623" : theme.horizon;
        ctx.fillRect(aiming ? 30 : 30, -4, aiming ? 6 : 4, aiming ? 8 : 8);
        ctx.restore();
        ctx.beginPath(); ctx.arc(launcherX, launcherY, 16, Math.PI, 0);
        const dg = ctx.createRadialGradient(launcherX, launcherY - 4, 2, launcherX, launcherY, 16);
        dg.addColorStop(0, "#3a3d50"); dg.addColorStop(1, "#1a1d2a");
        ctx.fillStyle = dg; ctx.fill(); ctx.strokeStyle = theme.horizon; ctx.lineWidth = 2; ctx.stroke();
    }

    function drawAimLine() {
        if (!aiming) return;
        ctx.setLineDash([4, 6]); ctx.strokeStyle = "rgba(245,166,35,0.55)"; ctx.lineWidth = 1.5; ctx.beginPath();
        const vx = curLaunchSpeed * Math.cos(aimAngle), vy = curLaunchSpeed * Math.sin(aimAngle);
        for (let i = 0; i <= 80; i++) {
            const t = i * 0.08, gs = getGameScale();
            const px = launcherX + vx * t * gs, py = launcherY - (vy * t - 0.5 * g * t * t) * gs;
            if (py > launcherY + 5 && i > 2) break; if (px > canvas.width) break;
            i === 0 ? ctx.moveTo(px, py) : ctx.lineTo(px, py);
        }
        ctx.stroke(); ctx.setLineDash([]);

        // power bar
        const bx = launcherX - 22, by = launcherY - 90, bw = 10, bh = 50;
        ctx.fillStyle = "rgba(0,0,0,0.5)"; ctx.beginPath(); ctx.roundRect(bx, by, bw, bh, 3); ctx.fill();
        ctx.strokeStyle = "rgba(255,255,255,0.2)"; ctx.lineWidth = 1; ctx.stroke();
        const pulse = 0.85 + Math.sin(frameCount * 0.1) * 0.15;
        const fill = Math.min(aimDist / 200, 1) * pulse;
        const pg = ctx.createLinearGradient(bx, by + bh, bx, by);
        pg.addColorStop(0, "#3de87a"); pg.addColorStop(0.5, "#f5a623"); pg.addColorStop(1, "#f55a6e");
        ctx.fillStyle = pg; ctx.beginPath(); ctx.roundRect(bx + 1, by + bh - bh * fill, bw - 2, bh * fill, 2); ctx.fill();

        const deg = (aimAngle * 180 / Math.PI).toFixed(0);
        const range = (curLaunchSpeed ** 2 * Math.sin(2 * aimAngle) / g).toFixed(0);
        ctx.fillStyle = "#f5a623"; ctx.font = "bold 12px 'JetBrains Mono',monospace"; ctx.fillText(`${deg}°`, launcherX + 40, launcherY - 45);
        ctx.fillStyle = "rgba(255,255,255,0.4)"; ctx.font = "10px 'JetBrains Mono',monospace"; ctx.fillText(`~${range}m`, launcherX + 40, launcherY - 30);
    }

    function drawParticles() {
        for (let i = particles.length - 1; i >= 0; i--) {
            const p = particles[i]; p.x += p.vx; p.y += p.vy; p.vy += 0.05; p.life--;
            if (p.life <= 0) { particles.splice(i, 1); continue; }
            ctx.globalAlpha = p.life / p.maxLife; ctx.fillStyle = p.color;
            ctx.beginPath(); ctx.arc(p.x, p.y, p.r * (p.life / p.maxLife), 0, Math.PI * 2); ctx.fill();
        }
        ctx.globalAlpha = 1;
    }

    function drawScorePopups() {
        for (let i = scorePopups.length - 1; i >= 0; i--) {
            const s = scorePopups[i]; s.y -= 1.2; s.life--;
            if (s.life <= 0) { scorePopups.splice(i, 1); continue; }
            ctx.globalAlpha = s.life / s.maxLife; ctx.fillStyle = "#3de87a";
            ctx.font = "bold 18px 'Inter',sans-serif";
            ctx.shadowBlur = 6; ctx.shadowColor = "#3de87a"; ctx.fillText(s.text, s.x, s.y); ctx.shadowBlur = 0;
        }
        ctx.globalAlpha = 1;
    }

    function update() {
        frameCount++;
        drawEnv(); drawLauncher(); drawAimLine(); drawParticles(); drawScorePopups();

        // HUD
        ctx.font = "bold 14px 'JetBrains Mono',monospace"; ctx.fillStyle = theme.horizon;
        ctx.fillText(planetName.toUpperCase(), 15, 24);
        ctx.fillStyle = "rgba(255,255,255,0.4)"; ctx.font = "12px 'JetBrains Mono',monospace";
        ctx.fillText(`g = ${g} m/s²`, 15, 42);
        ctx.fillStyle = "#f55a6e"; ctx.font = "14px 'JetBrains Mono',monospace";
        ctx.shadowBlur = 4; ctx.shadowColor = "#f55a6e";
        ctx.fillText(`LIVES: ${"♥ ".repeat(state.lives)}`, canvas.width - 180, 24); ctx.shadowBlur = 0;
        ctx.fillStyle = "#3de87a"; ctx.shadowBlur = 4; ctx.shadowColor = "#3de87a";
        ctx.fillText(`SCORE: ${state.score}`, canvas.width - 180, 44); ctx.shadowBlur = 0;

        if (state.activeBullets.length === 0 && state.lives > 0 && !aiming) {
            const p = 0.3 + (0.5 + Math.sin(frameCount * 0.04) * 0.5) * 0.2;
            ctx.fillStyle = "rgba(255,255,255,0.18)"; ctx.font = "13px 'Inter',sans-serif"; ctx.globalAlpha = p;
            ctx.fillText("Click & drag to aim, release to fire", canvas.width / 2 - 130, 25); ctx.globalAlpha = 1;
        }

        // walls
        walls.forEach(w => {
            const wg = ctx.createLinearGradient(w.x, w.y, w.x + w.w, w.y);
            wg.addColorStop(0, "#1e2030"); wg.addColorStop(1, "#282c40");
            ctx.fillStyle = wg; ctx.strokeStyle = theme.horizon + "88"; ctx.lineWidth = 1;
            ctx.beginPath(); ctx.roundRect(w.x, w.y, w.w, w.h, 3); ctx.fill(); ctx.stroke();
            ctx.fillStyle = theme.horizon; ctx.shadowBlur = 4; ctx.shadowColor = theme.horizon;
            ctx.fillRect(w.x + 1, w.y, w.w - 2, 2); ctx.shadowBlur = 0;
            ctx.strokeStyle = "rgba(255,255,255,0.04)";
            for (let ly = w.y + 10; ly < w.y + w.h; ly += 10) { ctx.beginPath(); ctx.moveTo(w.x + 2, ly); ctx.lineTo(w.x + w.w - 2, ly); ctx.stroke(); }
        });

        // targets hovering logic
        targets.forEach(t => {
            if (hitTargets.has(t.id)) return;
            const ty = t.y + Math.sin(frameCount * 0.03 + t.x * 0.02) * 15;
            t.renderY = ty; // Storing for bullet hit detection
            const pf = 0.7 + Math.sin(frameCount * 0.05 + t.x * 0.01) * 0.3;
            ctx.shadowBlur = 8 * pf; ctx.shadowColor = "#3de87a";
            const tg = ctx.createLinearGradient(t.x, ty, t.x, ty + t.h);
            tg.addColorStop(0, `rgba(61,232,122,${0.15 * pf})`); tg.addColorStop(1, `rgba(61,232,122,${0.05 * pf})`);
            ctx.fillStyle = tg; ctx.strokeStyle = "#3de87a"; ctx.lineWidth = 1.5;
            ctx.beginPath(); ctx.roundRect(t.x, ty, t.w, t.h, 5); ctx.fill(); ctx.stroke(); ctx.shadowBlur = 0;
            ctx.beginPath(); ctx.arc(t.x + t.w / 2, ty + t.h / 2, 6, 0, Math.PI * 2);
            ctx.strokeStyle = "#3de87a"; ctx.lineWidth = 1; ctx.stroke();
            ctx.beginPath(); ctx.arc(t.x + t.w / 2, ty + t.h / 2, 2, 0, Math.PI * 2); ctx.fillStyle = "#3de87a"; ctx.fill();
            ctx.fillStyle = "#3de87a"; ctx.font = "bold 9px 'JetBrains Mono',monospace"; ctx.fillText(t.id, t.x + 3, ty - 4);
        });

        // bullets - handling multiple active bullets
        for (let i = state.activeBullets.length - 1; i >= 0; i--) {
            const b = state.activeBullets[i];
            b.trail.push({ x: b.x, y: b.y }); if (b.trail.length > 50) b.trail.shift();
            const dt = 0.06, gs = getGameScale();
            b.x += b.vx * dt * gs; b.vy -= g * dt; b.y -= b.vy * dt * gs;

            b.trail.forEach((pt, j) => {
                const al = (j / b.trail.length) * 0.6, sz = 0.5 + (j / b.trail.length) * 3;
                ctx.beginPath(); ctx.arc(pt.x, pt.y, sz, 0, Math.PI * 2);
                ctx.fillStyle = `rgba(56,217,245,${al})`; ctx.fill();
            });
            ctx.beginPath(); ctx.arc(b.x, b.y, 5, 0, Math.PI * 2); ctx.fillStyle = "#fff";
            ctx.shadowBlur = 18; ctx.shadowColor = theme.horizon; ctx.fill(); ctx.shadowBlur = 0;
            ctx.beginPath(); ctx.arc(b.x, b.y, 8, 0, Math.PI * 2);
            ctx.strokeStyle = `${theme.horizon}44`; ctx.lineWidth = 1; ctx.stroke();

            let dead = false;
            walls.forEach(w => {
                if (b.x >= w.x && b.x <= w.x + w.w && b.y >= w.y && b.y <= w.y + w.h) {
                    dead = true;
                    for (let k = 0; k < 6; k++) particles.push({
                        x: b.x, y: b.y,
                        vx: (Math.random() - 0.5) * 3, vy: -Math.random() * 2 - 1, life: 20, maxLife: 20, color: "#8888aa", r: 1.5
                    });
                }
            });
            if (!dead) {
                targets.forEach(t => {
                    if (hitTargets.has(t.id)) return;
                    const ty = t.renderY || t.y;
                    if (b.x >= t.x - 6 && b.x <= t.x + t.w + 6 && b.y >= ty - 6 && b.y <= ty + t.h + 6) {
                        hitTargets.add(t.id); state.score += 100;
                        spawnExplosion(t.x + t.w / 2, ty + t.h / 2);
                        scorePopups.push({ x: t.x + 2, y: ty - 10, text: "+100", life: 50, maxLife: 50 });
                        dead = true; // Bullet disappears when hitting target
                    }
                });
            }
            if (dead || b.y >= groundY || b.x > canvas.width + 20 || b.x < -20) {
                state.activeBullets.splice(i, 1);
            }
        }

        // overlays
        if (hitTargets.size === targets.length && targets.length > 0) {
            ctx.fillStyle = "rgba(10,14,20,0.8)"; ctx.beginPath(); ctx.roundRect(canvas.width / 2 - 170, canvas.height / 2 - 35, 340, 70, 12); ctx.fill();
            ctx.strokeStyle = "#3de87a44"; ctx.lineWidth = 1; ctx.stroke();
            ctx.fillStyle = "#3de87a"; ctx.font = "bold 30px 'Inter',sans-serif";
            ctx.shadowBlur = 14; ctx.shadowColor = "#3de87a"; ctx.fillText("✓ LEVEL CLEAR!", canvas.width / 2 - 115, canvas.height / 2 + 8); ctx.shadowBlur = 0;
        } else if (state.lives <= 0 && state.activeBullets.length === 0) {
            ctx.fillStyle = "rgba(10,14,20,0.8)"; ctx.beginPath(); ctx.roundRect(canvas.width / 2 - 140, canvas.height / 2 - 35, 280, 70, 12); ctx.fill();
            ctx.strokeStyle = "#f55a6e44"; ctx.lineWidth = 1; ctx.stroke();
            ctx.fillStyle = "#f55a6e"; ctx.font = "bold 30px 'Inter',sans-serif";
            ctx.shadowBlur = 14; ctx.shadowColor = "#f55a6e"; ctx.fillText("GAME OVER", canvas.width / 2 - 95, canvas.height / 2 + 8); ctx.shadowBlur = 0;
        }

        gameAnimId = requestAnimationFrame(update);
    }

    const cleanup = () => {
        if (gameAnimId) { cancelAnimationFrame(gameAnimId); gameAnimId = null; }
        canvas.removeEventListener("mousedown", onMouseDown);
        canvas.removeEventListener("mousemove", onMouseMove);
        canvas.removeEventListener("mouseup", onMouseUp);
        canvas.removeEventListener("touchstart", onTouchStart);
        canvas.removeEventListener("touchmove", onTouchMove);
        canvas.removeEventListener("touchend", onTouchEnd);
    };

    update();
    return cleanup;
}

function stopGame() {
    if (gameAnimId) { cancelAnimationFrame(gameAnimId); gameAnimId = null; }
}

// ═══════════════════════════
//  Level generators
// ═══════════════════════════

function generateTargets(level, cw, groundY) {
    const count = Math.min(2 + level, 6), targets = [];
    const startX = cw * 0.2, endX = cw * 0.9, spacing = (endX - startX) / (count + 1);
    for (let i = 0; i < count; i++) {
        const x = startX + spacing * (i + 1), baseH = 25 + level * 6;
        targets.push({ id: `T${i + 1}`, x: Math.round(x), y: Math.round(Math.max(groundY - baseH - Math.random() * 25, 60)), w: 32, h: 28 });
    }
    return targets;
}

function generateWalls(level, cw, groundY) {
    const count = Math.min(Math.floor(level / 2) + 1, 4), walls = [];
    const startX = cw * 0.15, endX = cw * 0.75, spacing = (endX - startX) / (count + 2);
    for (let i = 0; i < count; i++) {
        const x = startX + spacing * (i + 1), h = 50 + level * 10 + Math.random() * 20;
        walls.push({ x: Math.round(x), y: Math.round(groundY - h), w: 16, h: Math.round(h) });
    }
    return walls;
}
