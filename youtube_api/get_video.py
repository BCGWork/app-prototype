import requests
import csv
import json
import os

CUR_DIR = os.getcwd()
f = open(os.path.abspath(os.path.join(CUR_DIR, "api_key.txt")), "r")
API_KEY = f.readlines()[0]

data = csv.writer(open("video_data.csv", "wb+"))
data.writerow(["video_id", "video_name", "upload_date"])
page = csv.writer(open("playlist_page.csv", "wb+"))
page.writerow(["page_id", "pageToken", "query_url"])

URL = "https://www.googleapis.com/youtube/v3/playlistItems"
args = {"part":"snippet", "playlistId":"UUsT0YIqwnpJCM-mx7-gSA4Q", "pageToken":None, "key":API_KEY, "maxResults":5}

pgCnt = 1
vCnt = 0
while pgCnt < 3:
	output = requests.get(URL, params=args)
	v = output.json()
	page.writerow([pgCnt, v["nextPageToken"], output.url])
	args["pageToken"] = v["nextPageToken"]
	print "Next page token is " + v["nextPageToken"]
	for item in v["items"]:
		dataRow = []
		dataRow.append(item["snippet"]["resourceId"]["videoId"])
		dataRow.append(item["snippet"]["title"].encode("utf-8"))
		dataRow.append(item["snippet"]["publishedAt"])
		data.writerow(dataRow)
		vCnt += 1
		print "video data recorded: " + str(vCnt)
	pgCnt += 1


# videoFile = open("video.json", "wb+")
# videoFile.write(output.text.encode("utf-8"))






