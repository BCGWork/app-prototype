import requests
import os

CUR_DIR = os.getcwd()
f = open(os.path.abspath(os.path.join(CUR_DIR, "api_key.txt")), "r")
API_KEY = f.readlines()[0]

URL = {}
args = {"api-key": API_KEY}
resources = ["countries", "events", "languages", "quotes", "ratings", "rating_words", "speakers", "states", "tedx_events", "tedx_groups", "tedx_speakers", "tedx_venues", "talks", "themes", "playlists"]
for item in resources:
	output = None
	URL[item] = "https://api.ted.com/v1/" + item + ".json?"
	output = requests.get(URL[item], params=args)
	jsonFile = open(item + ".json", "wb+")
	jsonFile.write(output.text)
	print item + ".json created."






