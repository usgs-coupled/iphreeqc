# pip install beautifulsoup4
#
# pip install --trusted-host pypi.org --trusted-host files.pythonhosted.org beautifulsoup4
#
import os
from bs4 import BeautifulSoup
from urllib.request import urlopen
from pathlib import Path

path = Path(os.path.join(os.getcwd(), 'html\IPhreeqc_8h.html'))

response = urlopen(path.as_uri())
soup = BeautifulSoup(response, 'html.parser')
#print(soup.prettify())
#print(soup.find_all('a', 'el'))
#print('\t\t<LI> <OBJECT type="text/sitemap">')
d = {}
for a in soup.find_all('a', 'el'):
    #print('{}={}'.format(a.text, a['href']))
    href = a['href']
    #f, h = href.split('#')
    f, h, l = href.partition('#')
    if f == 'IPhreeqc_8h.html' and len(l) == 33:
        d[a.text] = href

# remove 
d.pop('IPQ_RESULT')

for key in d.keys():
    print('{}={}'.format(key, d[key]))
