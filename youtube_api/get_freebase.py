import json
import urllib
import csv
import time

f = csv.reader(open("video_profile.csv", "r"), delimiter = ",")
f.next()
tList = []
for row in f:
	fb_id =row[3].split(",")
	tList += fb_id
topicList = set(tList)

api_key = open("api_key.txt").read()
service_url = 'https://www.googleapis.com/freebase/v1/topic'
params = {
  'key': api_key,
  'filter': "/type/object/name"
}

data = csv.writer(open("freebase_category.csv", "wb+"))
data.writerow(["id", "name"])
tCnt = 0
# data = csv.writer(open("freebase_tags.csv", "wb+"))
start_time = time.time()
for topic_id in topicList:
	# data.writerow([topic_id])
	if topic_id != "":
		url = service_url + topic_id + '?' + urllib.urlencode(params)
		topic = json.loads(urllib.urlopen(url).read())
		data.writerow([topic_id, topic['property']["/type/object/name"]['values'][0]['text'].encode("utf-8")])
		tCnt += 1
		print str(tCnt) + " - " + topic_id + " recorded."
elapsed_time = time.time() - start_time
print "Total elapsed time: " + str(elapsed_time) + " seconds"

