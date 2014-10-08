import requests
import csv
import json
import os
import time

CUR_DIR = os.getcwd()
f = open(os.path.abspath(os.path.join(CUR_DIR, "api_key.txt")), "r")
API_KEY = f.readlines()[0]

''' GET PROFILE OF ALL VIDEOS '''
vFile = csv.reader(open("video_data.csv", "r"), delimiter = ",")
vFile.next()
vList = []
for row in vFile:
	vList.append(row[0])
data = csv.writer(open("video_profile.csv", "wb+"))
data.writerow(["video_id", "video_detail", "youtube_category", "freebase_tag", "upload_date"])
URL = "https://www.googleapis.com/youtube/v3/videos"
args = {"part":"snippet,topicDetails", "id":None, "key":API_KEY, "maxResults":50}

vCnt = 0
iter = len(vList)/10
start_time = time.time()
for i in range(iter):
	args["id"] = ",".join(vList[(10*i+1):(10*i+11)])
	output = requests.get(URL, params=args)
	v = output.json()
	for item in v["items"]:
		dataRow = []
		dataRow.append(item["id"])
		dataRow.append(item["snippet"]["title"].encode("utf-8"))
		dataRow.append(item["snippet"]["categoryId"])
		if "topicDetails" in item.keys():
			if "topicIds" in item["topicDetails"].keys():
				dataRow.append(",".join(item["topicDetails"]["topicIds"]))
			else:
				dataRow.append(",".join(item["topicDetails"]["relevantTopicIds"]))
		else:
			dataRow.append("")
		dataRow.append(item["snippet"]["publishedAt"])
		data.writerow(dataRow)
	vCnt += 1
	print str(vCnt) + " - video profile recorded: " + args["id"]
elapsed_time = time.time() - start_time
print "Total elapsed time: " + str(elapsed_time) + " seconds"


''' GET LIST OF VIDEOS FROM PLAYLIST UPLOADED_TALKS '''
# data = csv.writer(open("video_data.csv", "wb+"))
# data.writerow(["video_id", "video_name", "upload_date"])
# page = csv.writer(open("playlist_page.csv", "wb+"))
# page.writerow(["page_id", "pageToken", "query_url"])
# URL = "https://www.googleapis.com/youtube/v3/playlistItems"
# args = {"part":"snippet", "playlistId":"UUsT0YIqwnpJCM-mx7-gSA4Q", "pageToken":None, "key":API_KEY, "maxResults":50}

# pgCnt = 1
# vCnt = 0
# nextPage = True
# start_time = time.time()
# while nextPage:
	# output = requests.get(URL, params=args)
	# v = output.json()
	# if "nextPageToken" in v:
		# page.writerow([pgCnt, v["nextPageToken"], output.url])
		# args["pageToken"] = v["nextPageToken"]
		# print "Next page token is " + v["nextPageToken"]
		# for item in v["items"]:
			# dataRow = []
			# dataRow.append(item["snippet"]["resourceId"]["videoId"])
			# dataRow.append(item["snippet"]["title"].encode("utf-8"))
			# dataRow.append(item["snippet"]["publishedAt"])
			# data.writerow(dataRow)
			# vCnt += 1
			# print "video data recorded: " + str(vCnt)
		# pgCnt += 1
	# else:
		# print "All videos recorded!"
		# nextPage = False
# elapsed_time = time.time() - start_time
# print "Total elapsed time: " + str(elapsed_time) + " seconds"


