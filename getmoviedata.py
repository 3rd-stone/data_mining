import requests
import json
import time
import random

def get_one_page(url):
    user_agent = 'Mozilla/4.0 (compatible; MSIE 5.5; Windows NT)'
    headers = { 'User-Agent' : user_agent }
    response = requests.get(url,headers=headers)
    if response.content:
        return response.text
    return None

def parse_one_page(html):
    data = json.loads(html)['cmts']
    for item in data:
        yield{
                'comment':item['content'],
                'data':item['time'].split(' ')[0],
                'rate':item['score'],
                'city':item['cityName'],
                'nickname':item['nickName']
                }
        
def save_to_txt():
    for i in range(1,1001):
        url = 'http://m.maoyan.com/mmdb/comment/s/movie/248566.json?_v_=yes&offset=' + str(i)
        html = get_one_page(url)
        #html = html.replace('<html><head></head><body><pre style="word-wrap: break-word; white-space: pre-wrap;">', '')
        #html = html.replace('</pre></body></html>', '')

        print('保存第%d页'%i)
        for item in parse_one_page(html):
            with open('dianying.txt','a',encoding='utf-8') as f:
                f.write(item['date']+','+item['nickName']+','+item['city']+','+str(item['rate'])+','+item['comment']+'\n')
        time.sleep(5+float(random.randint(1,100))/20)


def data_process(inputfile,outputfile):
    inopen = open(inputfile,'r',encoding='utf-8')
    outopen = open(outputfile,'w',encoding = 'utf-8')
    lines = inopen.readlines()
    list_l = []
    for line in lines:
        if line not in list_l:
            list_l.append(line)
            outopen.write(line)
    inopen.close()
    outopen.close()

if __name__=='__main__':
    save_to_txt()
    data_process('dianying.txt','outputdianying.txt')