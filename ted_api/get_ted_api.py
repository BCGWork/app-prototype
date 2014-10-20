import requests
import csv
import os
import time

CUR_DIR = os.getcwd()
f = open(os.path.abspath(os.path.join(CUR_DIR, "api_key.txt")), "r")
API_KEY = f.readlines()[0]
args = {"api-key": API_KEY}
args2 = {"api-key": API_KEY}


''' TEDXTALK TALK LOCATION '''
event_info = csv.writer(open("event_detail.csv", "wb+"))
event_info.writerow(["event_group_id", "event_name", "city", "country", "lat", "lng", "starts_at", "ends_at", "twitter_hashtag"])
events = csv.reader(open("ted_event.csv", "r"), delimiter = ",")
events.next()
eCnt = 0
start_time = time.time()
for row in events:
	args["filter"] = "name:" + row[0]
	url = "http://api.ted.com/v1/tedx_groups.json"
	output = requests.get(url, params=args)
	event_id = output.json()["tedx_groups"][0]["tedx_group"]["id"]
	event_name = row[0]
	
	args2["fields"] = "address,tedx_group,tedx_event_type_label,tags"
	args2["filter"] = "tedx_group_id:" + str(event_id)
	url2 = "http://api.ted.com/v1/tedx_event_locations.json"
	output2 = requests.get(url2, params=args2)
	if len(output2.json()["tedx_event_locations"]) > 0:
		city = output2.json()["tedx_event_locations"][0]["tedx_event_location"]["city"].encode("utf-8")
		lat = output2.json()["tedx_event_locations"][0]["tedx_event_location"]["lat"]
		lng = output2.json()["tedx_event_locations"][0]["tedx_event_location"]["lng"]
		country = output2.json()["tedx_event_locations"][0]["tedx_event_location"]["country_name"]
		starts_at = output2.json()["tedx_event_locations"][0]["tedx_event_location"]["starts_at"]
		ends_at = output2.json()["tedx_event_locations"][0]["tedx_event_location"]["ends_at"]
		twitter_hashtag = output2.json()["tedx_event_locations"][0]["tedx_event_location"]["twitter_codes"]
		
		event_info.writerow([event_id, event_name, city, country, lat, lng, starts_at, ends_at, twitter_hashtag])
		eCnt += 1
		print str(eCnt) + " - " + str(event_id) + " " + row[0] + " recorded"
elapsed_time = time.time() - start_time
print "Total elapsed time: " + str(elapsed_time) + " seconds"


''' TED API TEST '''
# resources = ["countries", "events", "languages", "quotes", "ratings", "rating_words", "speakers", "states", "tedx_events", "tedx_groups", "tedx_speakers", "tedx_venues", "talks", "themes", "playlists"]
# URL = {}
# for item in resources:
	# output = None
	# URL[item] = "https://api.ted.com/v1/" + item + ".json?"
	# output = requests.get(URL[item], params=args)
	# jsonFile = open(item + ".json", "wb+")
	# jsonFile.write(output.text)
	# print item + ".json created."

