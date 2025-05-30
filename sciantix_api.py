from flask import Flask, request, jsonify
import requests
import base64
import os

app = Flask(__name__)

# Configura i dati della tua repo
GITHUB_TOKEN = os.getenv("GITHUB_TOKEN")  # oppure scrivilo direttamente per test
GITHUB_REPO = "sciantix/sciantix-official"
BRANCH = "main"

HEADERS = {
    "Authorization": f"token {GITHUB_TOKEN}",
    "Accept": "application/vnd.github.v3+json"
}

@app.route("/get-file", methods=["GET"])
def get_file():
    filepath = request.args.get("path")
    if not filepath:
        return jsonify({"error": "Missing 'path' parameter"}), 400

    url = f"https://api.github.com/repos/{GITHUB_REPO}/contents/{filepath}?ref={BRANCH}"
    response = requests.get(url, headers=HEADERS)

    if response.status_code == 200:
        data = response.json()
        content = base64.b64decode(data["content"]).decode("utf-8")
        return jsonify({"content": content})
    else:
        return jsonify({"error": "File not found or access denied"}), response.status_code

if __name__ == "__main__":
    import os
    print(">>> Starting Flask app with dynamic port on Render <<<")
    port = int(os.environ.get("PORT", 5000))
    app.run(host="0.0.0.0", port=port, debug=False)
