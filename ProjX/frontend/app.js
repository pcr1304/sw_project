// ── ProjX App Controller ──
// Driven purely by the provided ProjX data (from AST evaluator).

(function () {
    "use strict";

    const canvas = document.getElementById("projx-canvas");
    const ctx = canvas.getContext("2d");
    const readout = document.getElementById("readout-bar");
    const dynamicTabs = document.getElementById("dynamic-tabs");

    let currentIdx = 0;
    let gameCleanup = null;

    let scenarios = [];
    if (typeof projxData !== "undefined") {
        scenarios = Array.isArray(projxData) ? projxData : [projxData];
    }

    // ═══════════════════════════
    //  Canvas sizing
    // ═══════════════════════════

    function resizeCanvas() {
        const wrap = canvas.parentElement;
        if (canvas.width !== wrap.clientWidth || canvas.height !== wrap.clientHeight) {
            canvas.width = wrap.clientWidth;
            canvas.height = wrap.clientHeight;
            render();
        }
    }

    const resizeObserver = new ResizeObserver(resizeCanvas);
    resizeObserver.observe(canvas.parentElement);
    window.addEventListener("resize", resizeCanvas);

    // ═══════════════════════════
    //  Tab Building
    // ═══════════════════════════

    function buildTabs() {
        if (!dynamicTabs) return;
        dynamicTabs.innerHTML = "";
        scenarios.forEach((scen, idx) => {
            const btn = document.createElement("button");
            btn.className = "tab-btn" + (idx === currentIdx ? " active" : "");
            btn.textContent = scen.label || (scen.mode === "game" ? "Game" : "Simulate");
            btn.addEventListener("click", () => {
                currentIdx = idx;
                buildTabs();
                render();
            });
            dynamicTabs.appendChild(btn);
        });
    }

    // ═══════════════════════════
    //  Readout bar
    // ═══════════════════════════

    function updateReadout(annos) {
        if (!annos || annos.length === 0) {
            readout.innerHTML = "";
            return;
        }
        readout.innerHTML = annos.map(a => {
            const cls = a.type === "range" ? "green" : (a.type === "max_height" ? "purple" : "amber");
            const label = a.type.replace("_", " ");
            return `<div class="readout-item">
                <span class="readout-label">${a.p} ${label}</span>
                <span class="readout-value ${cls}">${a.value.toFixed(2)} m</span>
            </div>`;
        }).join("");
    }

    // ═══════════════════════════
    //  Render dispatcher
    // ═══════════════════════════

    function render() {
        if (gameCleanup) { gameCleanup(); gameCleanup = null; }
        stopSimulation();
        stopGame();

        if (scenarios.length === 0) return;
        const scen = scenarios[currentIdx];
        document.body.className = "mode-" + scen.mode;

        if (scen.mode === "simulate") {
            const annotations = scen.annotations || [];
            updateReadout(annotations);
            // Pass launched=true to animate immediately
            scen.launched = true;
            renderSimulation(ctx, canvas, scen);
        } else if (scen.mode === "game") {
            readout.innerHTML = `
                <div class="readout-item">
                    <span class="readout-label">Planet</span>
                    <span class="readout-value amber">${(scen.planet || "earth").toUpperCase()}</span>
                </div>
                <div class="readout-item">
                    <span class="readout-label">Gravity</span>
                    <span class="readout-value">${scen.gravity || 9.8} m/s²</span>
                </div>
                <div class="readout-item">
                    <span class="readout-label">Click canvas to shoot!</span>
                    <span class="readout-value green">▶</span>
                </div>`;
            gameCleanup = renderGame(ctx, canvas, scen);
        }
    }

    // ═══════════════════════════
    //  Init
    // ═══════════════════════════

    buildTabs();
    requestAnimationFrame(() => {
        resizeCanvas();
    });

})();
