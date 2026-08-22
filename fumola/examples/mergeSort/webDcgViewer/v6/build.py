import json
from pathlib import Path

template = Path("index.html.template").read_text()
scene = json.loads(Path("scene.json").read_text())

scene_json = json.dumps(scene)

index = template.replace("103070301 /* SCENE_JSON */", scene_json)

Path("index.html").write_text(index)