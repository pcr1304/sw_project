// game.js
export function drawGame(data, canvas, ctx, groundY, scale) {
    let state = {
        lives: data.lives || 3,
        score: 0,
        activeBullet: null
    };

    const launchAngle = 45; 
    const launchSpeed = 12; 

    // 1. Generate random stars once so they don't flicker every frame
    const stars = [];
    for (let i = 0; i < 60; i++) {
        stars.push({
            x: Math.random() * canvas.width,
            y: Math.random() * groundY,
            r: Math.random() * 1.5,
            opacity: Math.random() * 0.5 + 0.1
        });
    }

    canvas.onclick = () => {
        if (!state.activeBullet && state.lives > 0) {
            const theta = launchAngle * (Math.PI / 180);
            state.activeBullet = {
                x: 40,
                y: groundY,
                vx: launchSpeed * Math.cos(theta),
                vy: launchSpeed * Math.sin(theta)
            };
            state.lives--;
        }
    };

    // Helper: Draw the Sky and Land
    function drawEnvironment() {
        // --- SKY ---
       
        let skyGrad = ctx.createLinearGradient(0, 0, 0, groundY);
        skyGrad.addColorStop(0, "#0b0c10"); 
        skyGrad.addColorStop(1, "#1c1e28");
        ctx.fillStyle = skyGrad;
        ctx.fillRect(0, 0, canvas.width, groundY);

        // Draw stars
        stars.forEach(s => {
            ctx.fillStyle = `rgba(255, 255, 255, ${s.opacity})`;
            ctx.beginPath();
            ctx.arc(s.x, s.y, s.r, 0, Math.PI * 2);
            ctx.fill();
        });

        // --- LAND ---
        ctx.fillStyle = "#13141a"; // Surface color
        ctx.fillRect(0, groundY, canvas.width, canvas.height - groundY);
        
        // Glowing horizon line
        ctx.beginPath();
        ctx.moveTo(0, groundY);
        ctx.lineTo(canvas.width, groundY);
        ctx.strokeStyle = "#38d9f5";
        ctx.lineWidth = 1.5;
        ctx.stroke();
    }

    // Draw the Launcher Cannon
    function drawLauncher() {
        const x = 40;
        const y = groundY;
        const theta = launchAngle * (Math.PI / 180);

        // Save the unrotated canvas state
        ctx.save();
        
        // Move the canvas origin to the launcher's pivot point
        ctx.translate(x, y);
        // Rotate the canvas upwards (negative angle because Y grows downwards)
        ctx.rotate(-theta); 

        // Draw the Barrel
        ctx.fillStyle = "rgba(56,217,245,0.7)"; 
        ctx.strokeStyle = "#38d9f5";
        ctx.lineWidth = 1.5;
        // The rect is offset so it pivots around the back center
        ctx.fillRect(0, -6, 25, 12);
        ctx.strokeRect(0, -6, 25, 12);

        // Restore the canvas 
        ctx.restore(); 

        // Dome Base 
        ctx.beginPath();
        ctx.arc(x, y, 12, Math.PI, 0); 
        ctx.fillStyle = "#2a2d3a";
        ctx.fill();
        ctx.strokeStyle = "#38d9f5";
        ctx.lineWidth = 2;
        ctx.stroke();
    }

    function update() {
        // new scenery
        drawEnvironment();
        drawLauncher();

        // UI: Planet & Lives
        ctx.fillStyle = "#f55a6e";
        ctx.font = "12px monospace";
        ctx.fillText(`${data.planet.toUpperCase()} g=${data.gravity}`, 15, 25);
        
        ctx.fillStyle = "#f5a623";
        let hearts = Array(state.lives).fill("♥").join(" ");
        ctx.fillText(`LIVES: ${hearts}`, canvas.width - 100, 25);

        // (Obstacles)
        if (data.walls) {
            ctx.fillStyle = "#2a2d3a";
            ctx.strokeStyle = "#38d9f5";
            ctx.lineWidth = 1;
            data.walls.forEach(w => {
               
                ctx.beginPath();
                ctx.roundRect(w.x, w.y, w.w, w.h, 4); 
                ctx.fill();
                ctx.stroke();
            });
        }

        // Targets
        if (data.targets) {
            ctx.fillStyle = "rgba(61,232,122,0.15)";
            ctx.strokeStyle = "#3de87a";
            ctx.lineWidth = 1.5;
            data.targets.forEach(t => {
                ctx.beginPath();
                ctx.roundRect(t.x, t.y, t.w, t.h, 4);
                ctx.fill();
                ctx.stroke();
                
                // label to the targets
                ctx.fillStyle = "#3de87a";
                ctx.font = "10px monospace";
                ctx.fillText(t.id, t.x + 8, t.y + 16);
            });
        }

        // Bullet
        if (state.activeBullet) {
            let b = state.activeBullet;
            b.x += b.vx * 0.15 * scale; 
            b.vy -= data.gravity * 0.15;
            b.y -= b.vy * 0.15 * scale; 

            // Glowing bullet effect
            ctx.beginPath();
            ctx.arc(b.x, b.y, 4, 0, Math.PI * 2);
            ctx.fillStyle = "#ffffff";
            ctx.shadowBlur = 10;
            ctx.shadowColor = "#38d9f5";
            ctx.fill();
            
            // Reset shadow 
            ctx.shadowBlur = 0; 

            if (b.y >= groundY) {
                state.activeBullet = null;
            }
        }

        requestAnimationFrame(update);
    }

    update();
}