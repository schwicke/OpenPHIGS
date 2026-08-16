import urllib.request, json
req = urllib.request.Request('https://api.github.com/repos/CERN/OpenPHIGS/pulls/51/commits')
req.add_header('Accept', 'application/vnd.github.v3+json')
with urllib.request.urlopen(req) as response:
    commits = json.loads(response.read().decode())
    last_commit = commits[-1]['sha']

req = urllib.request.Request(f'https://api.github.com/repos/CERN/OpenPHIGS/commits/{last_commit}/check-runs')
req.add_header('Accept', 'application/vnd.github.v3+json')
with urllib.request.urlopen(req) as response:
    checks = json.loads(response.read().decode())
    for check in checks['check_runs']:
        if check['conclusion'] == 'failure':
            print(f"Failed: {check['name']} - {check['html_url']}")
            # Fetch the raw logs if possible, but github actions logs require auth or downloading a zip
            # Let's just print the URL so we know which jobs failed
