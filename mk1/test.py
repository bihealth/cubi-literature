import json
from scholarly import scholarly
#from scholarly import ProxyGenerator
from random import randrange
import time
import re

# Set up a ProxyGenerator object to use free proxies
# This needs to be done only once per session
#pg = ProxyGenerator()
#pg.FreeProxies()
#scholarly.use_proxy(pg)

# Now search Google Scholar from behind a proxy
search_query = scholarly.search_pubs('Spermidine reduces neuroinflammation and soluble amyloid beta in an Alzheimers disease mouse model. ')
#print(json.dumps(next(search_query)))

curline = ""
cnt = 1
pubs = [ ]

fp = open("publications.txt", "r")
for line in fp:
    #print(line)
    if re.search("^ *$", line):
        if curline != "":
            pubs.append(curline)
            print("")
            print("found pub:")
            print(curline)
            title = re.sub(r".*[^*]\*([^*].*[^*])\*[^*].*", r"\1", curline)
            print("title: " + title)
        curline = ""
        continue
    if curline != "":
        curline += " "
    curline += line.strip()
    cnt += 1

fp.close()

output = open("pubs_found.json", "a")
for pub in pubs:
  print("pub: ", pub)
  title = re.sub(r".*[^*]\*([^*].*[^*])\*[^*].*", r"\1", pub)
  print("title: ", title, "\n")
  query = scholarly.search_pubs(title)
  result = next(query)
  if result:
      #result = scholarly.fill(result)
      result_js = json.dumps(result, indent=2)
      print(result_js)
      output.write(result_js)
  print("sleeping...\n")
  time.sleep(2 + randrange(4))

output.close()


#search_query = scholarly.search_pubs('Spermidine reduces neuroinflammation and soluble amyloid beta in an Alzheimers disease mouse model. ')
