import csv
import time
import json

start_time = time.time()

video_profile = csv.reader(open("video_profile.csv", "r"), delimiter = ",")
video_country = csv.reader(open("analytics_country_video.csv", "r"), delimiter = ",")
video_profile.next()
video_country.next()

video_info = {}
vCnt = 0
currentVideo = ""
for line in video_profile:
	video_info[line[0]] = {"country":[], "uploadDate":line[4][0:10]}
for line in video_country:
	if line[0] in video_info.keys():
		video_info[line[0]]["country"].append(line[1])
		if currentVideo != line[0]:
			vCnt += 1
			currentVideo = line[0]
			print str(vCnt) + " videos processed"

f = open("video_country_month.json", "wb+")
f.write(json.dumps(video_info))
elapsed_time = time.time() - start_time
print "Total elapsed time: " + str(elapsed_time) + " seconds"

