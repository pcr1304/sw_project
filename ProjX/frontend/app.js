// ── ProjX App Controller ──
// Happening with screen movements ----> Tab switching, control bindings, data flow between UI and canvas.

(function () {
    "use strict";

    // ═══════════════════════════
    //  State
    // ═══════════════════════════

    const planetGravity = {
        earth: 9.80, moon: 1.62, mars: 3.72,
        jupiter: 24.8, saturn: 10.44
    };

    const planetImages = {
        earth: "bgearth.jpeg",
        moon: "planet1.jpeg",
        mars: "mars.png",
        jupiter: "jupiter.png",
        saturn: "saturn.png"
    };

    let currentMode = "simulate";
    let gameCleanup = null;

    // Simulation state — editable via UI
    let simState = {
        gravity: 9.8,
        projectiles: [
            { id: "p1", angle: 45, speed: 30, launch_from: [0, 0] }
        ]
    };

    // Game state
    let gameState = {
        planet: "earth",
        gravity: 9.8,
        level: 1,
        lives: 3,
        launchAngle: 45,
        launchSpeed: 30
    };

    // Load initial data from emitter if available
    if (typeof projxData !== 'undefined') {
        if (projxData.mode === "simulate") {
            simState.gravity = projxData.gravity || 9.8;
            simState.projectiles = projxData.projectiles || simState.projectiles;
        } else if (projxData.mode === "game") {
            gameState.planet = projxData.planet || "earth";
            gameState.gravity = projxData.gravity || 9.8;
            gameState.level = projxData.level || 1;
            gameState.lives = projxData.lives || 3;
        }
    }

    // ═══════════════════════════
    //  DOM refs
    // ═══════════════════════════

    const canvas = document.getElementById("projx-canvas");
    const ctx = canvas.getContext("2d");
    const readout = document.getElementById("readout-bar");
    const projList = document.getElementById("proj-list");
    const tabBtns = document.querySelectorAll(".tab-btn");

    // sim controls
    const simGravSlider = document.getElementById("sim-gravity");
    const simGravVal = document.getElementById("sim-gravity-val");
    const simLaunchBtn = document.getElementById("sim-launch-btn");

    let isSimLaunched = false;

    // game controls
    const planetSel = document.getElementById("planet-select");
    const planetPreview = document.getElementById("planet-preview");
    const planetGravLbl = document.getElementById("planet-gravity-label");
    const gameLevelSldr = document.getElementById("game-level");
    const gameLevelVal = document.getElementById("game-level-val");
    const gameLivesSldr = document.getElementById("game-lives");
    const gameLivesVal = document.getElementById("game-lives-val");
    const gameAngleSldr = null; // removed — aiming is via mouse drag
    const gameAngleVal = null;
    const gameSpeedSldr = null;
    const gameSpeedVal = null;

    // add projectile
    const addProjBtn = document.getElementById("add-proj");

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

    const resizeObserver = new ResizeObserver(() => {
        resizeCanvas();
    });
    resizeObserver.observe(canvas.parentElement);
    window.addEventListener("resize", resizeCanvas);

    // ═══════════════════════════
    //  Tab switching
    // ═══════════════════════════

    tabBtns.forEach(btn => {
        btn.addEventListener("click", () => {
            tabBtns.forEach(b => b.classList.remove("active"));
            btn.classList.add("active");
            currentMode = btn.dataset.mode;
            document.body.className = "mode-" + currentMode;
            isSimLaunched = false;
            render();
        });
    });

    // ═══════════════════════════
    //  Simulation controls
    // ═══════════════════════════

    simGravSlider.value = simState.gravity;
    simGravVal.textContent = simState.gravity;

    simGravSlider.addEventListener("input", () => {
        simState.gravity = parseFloat(simGravSlider.value);
        simGravVal.textContent = simState.gravity.toFixed(1);
        isSimLaunched = false;
        render();
    });

    if (simLaunchBtn) {
        simLaunchBtn.addEventListener("click", () => {
            isSimLaunched = true;
            render();
        });
    }

    // ── Projectile list UI ──

    function buildProjList() {
        projList.innerHTML = "";
        simState.projectiles.forEach((p, i) => {
            const color = COLORS[i % COLORS.length];
            const card = document.createElement("div");
            card.className = "proj-card";
            card.innerHTML = `
                <div class="proj-card-header">
                    <span class="proj-name"><span class="proj-dot" style="background:${color}"></span>${p.id}</span>
                    <button class="remove-proj" data-idx="${i}" title="Remove">✕</button>
                </div>
                <div class="control-group">
                    <div class="control-label"><span>Angle</span><span class="control-value cv-angle-${i}">${p.angle}°</span></div>
                    <input type="range" class="proj-angle" data-idx="${i}" min="1" max="89" step="1" value="${p.angle}">
                </div>
                <div class="control-group">
                    <div class="control-label"><span>Speed</span><span class="control-value cv-speed-${i}">${p.speed}</span></div>
                    <input type="range" class="proj-speed" data-idx="${i}" min="1" max="100" step="0.5" value="${p.speed}">
                </div>
                <div class="control-group">
                    <div class="control-label"><span>X₀</span><span class="control-value cv-x0-${i}">${p.launch_from[0]}</span></div>
                    <input type="range" class="proj-x0" data-idx="${i}" min="0" max="100" step="1" value="${p.launch_from[0]}">
                </div>
                <div class="control-group">
                    <div class="control-label"><span>Y₀</span><span class="control-value cv-y0-${i}">${p.launch_from[1]}</span></div>
                    <input type="range" class="proj-y0" data-idx="${i}" min="0" max="50" step="1" value="${p.launch_from[1]}">
                </div>
            `;
            projList.appendChild(card);
        });

        // bind slider events
        projList.querySelectorAll(".proj-angle").forEach(el => {
            el.addEventListener("input", () => {
                const i = parseInt(el.dataset.idx);
                simState.projectiles[i].angle = parseFloat(el.value);
                document.querySelector(`.cv-angle-${i}`).textContent = el.value + "°";
                isSimLaunched = false;
                render();
            });
        });
        projList.querySelectorAll(".proj-speed").forEach(el => {
            el.addEventListener("input", () => {
                const i = parseInt(el.dataset.idx);
                simState.projectiles[i].speed = parseFloat(el.value);
                document.querySelector(`.cv-speed-${i}`).textContent = el.value;
                isSimLaunched = false;
                render();
            });
        });
        projList.querySelectorAll(".proj-x0").forEach(el => {
            el.addEventListener("input", () => {
                const i = parseInt(el.dataset.idx);
                simState.projectiles[i].launch_from[0] = parseFloat(el.value);
                document.querySelector(`.cv-x0-${i}`).textContent = el.value;
                isSimLaunched = false;
                render();
            });
        });
        projList.querySelectorAll(".proj-y0").forEach(el => {
            el.addEventListener("input", () => {
                const i = parseInt(el.dataset.idx);
                simState.projectiles[i].launch_from[1] = parseFloat(el.value);
                document.querySelector(`.cv-y0-${i}`).textContent = el.value;
                isSimLaunched = false;
                render();
            });
        });
        projList.querySelectorAll(".remove-proj").forEach(el => {
            el.addEventListener("click", () => {
                const i = parseInt(el.dataset.idx);
                if (simState.projectiles.length <= 1) return;
                simState.projectiles.splice(i, 1);
                // re-number ids
                simState.projectiles.forEach((p, j) => p.id = "p" + (j + 1));
                buildProjList();
                isSimLaunched = false;
                render();
            });
        });
    }

    addProjBtn.addEventListener("click", () => {
        const n = simState.projectiles.length + 1;
        simState.projectiles.push({
            id: "p" + n,
            angle: 30 + Math.random() * 30,
            speed: 20 + Math.random() * 20,
            launch_from: [0, 0]
        });
        // floor values for display
        const last = simState.projectiles[simState.projectiles.length - 1];
        last.angle = Math.round(last.angle);
        last.speed = Math.round(last.speed);
        buildProjList();
        isSimLaunched = false;
        render();
    });

    // ═══════════════════════════
    //  Game controls
    // ═══════════════════════════

    const themeColors = {
        earth: "#38d9f5", moon: "#bbbbcc", mars: "#e87040",
        jupiter: "#f5b040", saturn: "#c8a060"
    };

    function updatePlanetPreview() {
        const planet = planetSel.value;
        planetPreview.style.backgroundImage = `url('${planetImages[planet] || planetImages.earth}')`;
        const g = planetGravity[planet] || 9.8;
        planetGravLbl.textContent = g + " m/s²";
        gameState.planet = planet;
        gameState.gravity = g;

        // update preview ring color to match planet theme
        const col = themeColors[planet] || "#38d9f5";
        planetPreview.style.borderColor = col;
        planetPreview.style.boxShadow = `0 0 12px ${col}40`;
        planetGravLbl.style.color = col;
    }

    planetSel.addEventListener("change", () => {
        updatePlanetPreview();
        render();
    });

    gameLevelSldr.addEventListener("input", () => {
        gameState.level = parseInt(gameLevelSldr.value);
        gameLevelVal.textContent = gameState.level;
        render();
    });

    gameLivesSldr.addEventListener("input", () => {
        gameState.lives = parseInt(gameLivesSldr.value);
        gameLivesVal.textContent = gameState.lives;
        render();
    });

    // New Game button
    const newGameBtn = document.getElementById("new-game-btn");
    if (newGameBtn) {
        newGameBtn.addEventListener("click", () => {
            if (currentMode === "game") render();
        });
    }

    // ═══════════════════════════
    //  Physics for readout
    // ═══════════════════════════

    function computeAnnotations(g, projs) {
        const annos = [];
        projs.forEach(p => {
            const theta = p.angle * (Math.PI / 180);
            const x0 = p.launch_from ? p.launch_from[0] : 0;
            const y0 = p.launch_from ? p.launch_from[1] : 0;
            const r = (p.speed * p.speed * Math.sin(2 * theta)) / g + x0;
            const mh = (p.speed * Math.sin(theta)) ** 2 / (2 * g) + y0;
            annos.push({ type: "range", p: p.id, value: r });
            annos.push({ type: "max_height", p: p.id, value: mh });
        });
        return annos;
    }

    // ═══════════════════════════
    //  Readout bar
    // ═══════════════════════════

    function updateReadout(annos) {
        readout.innerHTML = annos.map(a => {
            const cls = a.type === "range" ? "green" : a.type === "max_height" ? "purple" : "amber";
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
        // cleanup previous game
        if (gameCleanup) { gameCleanup(); gameCleanup = null; }
        stopSimulation();
        stopGame();

        if (currentMode === "simulate") {
            const annos = computeAnnotations(simState.gravity, simState.projectiles);
            const data = {
                gravity: simState.gravity,
                projectiles: simState.projectiles,
                annotations: annos,
                launched: isSimLaunched
            };
            updateReadout(annos);
            renderSimulation(ctx, canvas, data);
        } else {
            readout.innerHTML = `
                <div class="readout-item">
                    <span class="readout-label">Planet</span>
                    <span class="readout-value amber">${gameState.planet.toUpperCase()}</span>
                </div>
                <div class="readout-item">
                    <span class="readout-label">Gravity</span>
                    <span class="readout-value">${gameState.gravity} m/s²</span>
                </div>
                <div class="readout-item">
                    <span class="readout-label">Click canvas to shoot!</span>
                    <span class="readout-value green">▶</span>
                </div>`;
            gameCleanup = renderGame(ctx, canvas, gameState);
        }
    }

    // ═══════════════════════════
    //  Init
    // ═══════════════════════════

    updatePlanetPreview();
    buildProjList();
    // use requestAnimationFrame to ensure canvas has dimensions
    requestAnimationFrame(() => {
        resizeCanvas();
    });

})();
