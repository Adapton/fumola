import json
import sys
from pathlib import Path

template = Path("index.template.html").read_text()
# `python3 build.py [scene.json]`. The default is the 44-element mergeSort run;
# `scenes/` holds others, among them a realignment -- one graph, one edit, one
# repair -- written by `scripts/write-realign-scene.sh`.
scene = json.loads(Path(sys.argv[1] if len(sys.argv) > 1 else "scene.json").read_text())

scene_json = json.dumps(scene)

index = template.replace("103070301 /* SCENE_JSON */", scene_json)

Path("index.html").write_text(index)
