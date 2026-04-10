const projxData = [
  {
    "label": "Sim (g=9.8)",
    "mode": "simulate",
    "gravity": 9.8000,
    "projectiles": [
      {"id":"p1", "angle":45, "speed":30, "launch_from":[0,0]},
      {"id":"p2", "angle":60, "speed":25, "launch_from":[0,0]}
    ],
    "annotations": [
      {"type":"range", "p":"p1", "value":91.8367},
      {"type":"max_height", "p":"p1", "value":22.9592}
    ],
    "bounces": [
      {"p":"p1", "arcs":[[0,0,45,30],[91.8367,0,45,21],[136.8367,0,45,14.7000]]}
    ],
    "collisions": [
      {"p1":"p1", "p2":"p2", "t":0, "x":0, "y":0}
    ]
},
  {
    "label": "Fork: Earth (g=9.8)",
    "mode": "simulate",
    "gravity": 9.8000,
    "projectiles": [
      {"id":"p1", "angle":45, "speed":30, "launch_from":[0,0]}
    ],
    "annotations": [
      {"type":"range", "p":"p1", "value":91.8367},
      {"type":"max_height", "p":"p1", "value":22.9592}
    ],
    "bounces": [
      {"p":"p1", "arcs":[[0,0,45,30],[91.8367,0,45,21]]}
    ],
    "collisions": [

    ]
},
  {
    "label": "Fork: Mars (g=3.7)",
    "mode": "simulate",
    "gravity": 3.7200,
    "projectiles": [
      {"id":"p1", "angle":45, "speed":30, "launch_from":[0,0]}
    ],
    "annotations": [
      {"type":"range", "p":"p1", "value":241.9355},
      {"type":"max_height", "p":"p1", "value":60.4839}
    ],
    "bounces": [

    ],
    "collisions": [

    ]
},
  {
    "label": "Sim (g=9.8)",
    "mode": "simulate",
    "gravity": 9.8000,
    "projectiles": [
      {"id":"p1", "angle":45, "speed":30, "launch_from":[0,0]},
      {"id":"p2", "angle":60, "speed":25, "launch_from":[0,0]},
      {"id":"p", "angle":20, "speed":40, "launch_from":[0,0]}
    ],
    "annotations": [
      {"type":"range", "p":"p", "value":104.9449},
      {"type":"max_height", "p":"p", "value":9.5492}
    ],
    "bounces": [

    ],
    "collisions": [

    ]
},
  {
    "label": "Sim (g=9.8)",
    "mode": "simulate",
    "gravity": 9.8000,
    "projectiles": [
      {"id":"p1", "angle":45, "speed":30, "launch_from":[0,0]},
      {"id":"p2", "angle":60, "speed":25, "launch_from":[0,0]},
      {"id":"p", "angle":30, "speed":40, "launch_from":[0,0]}
    ],
    "annotations": [
      {"type":"range", "p":"p", "value":141.3919},
      {"type":"max_height", "p":"p", "value":20.4082}
    ],
    "bounces": [

    ],
    "collisions": [

    ]
},
  {
    "label": "Sim (g=9.8)",
    "mode": "simulate",
    "gravity": 9.8000,
    "projectiles": [
      {"id":"p1", "angle":45, "speed":30, "launch_from":[0,0]},
      {"id":"p2", "angle":60, "speed":25, "launch_from":[0,0]},
      {"id":"p", "angle":40, "speed":40, "launch_from":[0,0]}
    ],
    "annotations": [
      {"type":"range", "p":"p", "value":160.7849},
      {"type":"max_height", "p":"p", "value":33.7286}
    ],
    "bounces": [

    ],
    "collisions": [

    ]
},
  {
    "label": "Sim (g=9.8)",
    "mode": "simulate",
    "gravity": 9.8000,
    "projectiles": [
      {"id":"p1", "angle":45, "speed":30, "launch_from":[0,0]},
      {"id":"p2", "angle":60, "speed":25, "launch_from":[0,0]},
      {"id":"p", "angle":50, "speed":40, "launch_from":[0,0]}
    ],
    "annotations": [
      {"type":"range", "p":"p", "value":160.7849},
      {"type":"max_height", "p":"p", "value":47.9040}
    ],
    "bounces": [

    ],
    "collisions": [

    ]
},
  {
    "label": "Sim (g=9.8)",
    "mode": "simulate",
    "gravity": 9.8000,
    "projectiles": [
      {"id":"p1", "angle":45, "speed":30, "launch_from":[0,0]},
      {"id":"p2", "angle":60, "speed":25, "launch_from":[0,0]},
      {"id":"p", "angle":60, "speed":40, "launch_from":[0,0]}
    ],
    "annotations": [
      {"type":"range", "p":"p", "value":141.3919},
      {"type":"max_height", "p":"p", "value":61.2245}
    ],
    "bounces": [

    ],
    "collisions": [

    ]
},
  {
    "label": "Sim (g=9.8)",
    "mode": "simulate",
    "gravity": 9.8000,
    "projectiles": [
      {"id":"p1", "angle":45, "speed":30, "launch_from":[0,0]},
      {"id":"p2", "angle":60, "speed":25, "launch_from":[0,0]},
      {"id":"p", "angle":70, "speed":40, "launch_from":[0,0]}
    ],
    "annotations": [
      {"type":"range", "p":"p", "value":104.9449},
      {"type":"max_height", "p":"p", "value":72.0834}
    ],
    "bounces": [

    ],
    "collisions": [

    ]
},
  {
    "label": "Game: mars Lv3",
    "mode": "game",
    "planet": "mars",
    "gravity": 3.7200,
    "level": 3,
    "lives": 5,
    "targets": [],
    "walls": []
}
];
