# main.py
import requests
from bs4 import BeautifulSoup
from time import sleep
import pandas as pd
import os
from urllib import parse

# step1
# 네이버 영화 메인 페이지에서 1 ~ 10까지의 영화의
# 상세페이지 주소를 스크래핑한다.
def step1_get_detail_url() :
    # 접속할 페이지의 주소
    site = 'https://movie.naver.com/movie/running/current.nhn?order=reserve'
    # 접속한다.
    response = requests.get(site)
    bs = BeautifulSoup(response.content, 'html.parser')
    # print(bs)
    # a 태그들을 가져온다.
    a_list = bs.select('.top_thumb_lst a')

    # href 속성을 가져온다.
    df = pd.DataFrame()
    for idx in range(10) :
        href = a_list[idx].get('href')
        # 가져온 href 속성의 주소를 분석한 객체를 생성한다.
        a1 = parse.urlparse(href)
        # 주소를 분석한 객체서 쿼리 스트링을 가져온다(? 이후)
        query_str = parse.parse_qs(a1.query)
        # 추출한 쿼리스트링 데이터에서 원하는 파라미터 데이터를
        # 추출한다.
        code = query_str['code'][0]
        print(code)


        # print(href)
        df = df.append([[code]], ignore_index=True)

    df.columns = ['code']
    df.to_csv('movie_code_list.csv', index=False, encoding='utf-8-sig')
    print('주소 저장 완료')

# step2
# 상세페이지에서 평점의 더보기를 클릭했을 때 보여지는
# 페이지의 주소를 스크래핑한다.
def step2_get_reple_href() :
    # 스크랩한 영화 코드를 불러온다.
    code_frame = pd.read_csv('movie_code_list.csv')
    code_list = code_frame['code'].tolist()

    # 테스트용
    code_list = ['156464', '109906']

    # 영화코드와 주소를 합쳐서 요청할 주소를 만든다.
    url_list = pd.DataFrame()
    for code in code_list:
        site = 'https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code=%s&type=after&isActualPointWriteExecute=false&isMileageSubscriptionAlready=false&isMileageSubscriptionReject=false' % code
        # print(site)
        url_list = url_list.append([[site]], ignore_index=True)

    url_list.columns = ['url']
    url_list.to_csv('movie_url_list.csv', index=False, encoding='utf-8-sig')
    print('저장완료')


# step3
# 140자 평을 1페이지부터 마지막 페이지까지 순회하면서
# 평점점수와 140자평 데이터를 스크래핑한다.
def step3_get_reply_data() :
    # csv에 저장되어 있는 url 데이터를 가져온다.
    df = pd.read_csv('movie_url_list.csv')
    url_list = df['url'].tolist()

    # for url in url_list :
    #    print(url)
    # url = url_list[0]

    for url in url_list :
        print(url)
        # 해당 영화의 첫 페이지 html 데이터를 가져온다.
        response = requests.get(url)
        bs = BeautifulSoup(response.content, 'html.parser')
        # print(bs)
        # 총 페이지 수를 구한다.
        strong = bs.select('.total em')
        score_total = int(strong[1].text.replace(',', ''))
        # print(score_total)
        pageCnt = score_total // 10
        if score_total % 10 > 0 :
            pageCnt += 1
        # print(pageCnt)

        # 전체 페이지를 돌면서 140평 데이터를 가져온다.
        # 현재 페이지
        now_page = 1

        # pageCnt = 5
        while now_page <= pageCnt :
            sleep(1)
            # 요청할 페이지의 주소
            url2 = url + '&page=' + str(now_page)
            # print(url2)

            # 140자평 데이터를 추출한다.
            response2 = requests.get(url2)
            bs2 = BeautifulSoup(response2.content, 'html.parser')

            result_df = pd.DataFrame()

            # li 태그들을 가져온다.
            lis = bs2.select('.score_result li')

            for obj in lis :
                # 평점
                star_score = obj.select('.star_score em')[0].text
                # 140자평
                score_reple = obj.select('.score_reple p')[0].text
                # print(star_score)
                # print(score_reple)

                # 저장한다.

                result_df = result_df.append([[score_reple, star_score]], ignore_index=True)

            if os.path.exists('star_score.csv') == False :
                result_df.columns = ['text', 'score']
                result_df.to_csv('star_score.csv', index=False, encoding='utf-8-sig')
            else :
                result_df.to_csv('star_score.csv', index=False, encoding='utf-8-sig', mode='a', header=False)

            print("%d / %d" % (now_page, pageCnt))
            now_page += 1

    print('저장완료')


# 140자평 데이터 전처리 함수
def text_preprocessing(text) :
    if text.startswith('관람객') :
        return text[3:]
    else :
        return text

# 평점 전처리 함수
def star_preprocessing(text) :
    value = int(text)

    if value <= 5 :
        return '0'
    else :
        return '1'

# step4
# 학습을 위해 데이터 전처리를 수행한다.
def step4_data_preprocessing() :
    # 수집한 데이터를 읽어온다.
    df = pd.read_csv('star_score.csv')
    # 전처리를 수행한다.
    df['text'] = df['text'].apply(text_preprocessing)
    df['score'] = df['score'].apply(star_preprocessing)

    # 학습데이터와 테스트 데이터로 나눈다.
    text_list = df['text'].tolist()
    star_list = df['score'].tolist()

    from sklearn.model_selection import train_test_split

    text_train, text_test, star_train, star_test = train_test_split(text_list, star_list, test_size=0.3, random_state=0)

    return text_train, text_test, star_train, star_test


from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.linear_model import LogisticRegression
from sklearn.pipeline import Pipeline
from sklearn.metrics import accuracy_score
import pickle
from konlpy.tag import *

# 형태소 분석을 위한 함수
def tokenizer(text) :
    okt = Okt()
    return okt.morphs(text)

# step5
# 학습을 한다. 학습이 완료된 모델을 파일로 저장한다.
def step5_learning(X_train, y_train, X_test, y_test):
    # 주어진 데이터를 단어 사전으로 만들고
    # 각 단어의 빈도수를 계산한 후 벡터화 하는 객체 생성
    tfidf = TfidfVectorizer(lowercase=False, tokenizer=tokenizer)
    logistic = LogisticRegression(C=10.0, penalty='l2', random_state=0)

    pipe = Pipeline([('vect', tfidf), ('clf', logistic)])

    # 학습한다.
    pipe.fit(X_train, y_train)

    # 학습 정확도 측정
    y_pred = pipe.predict(X_test)
    print(accuracy_score(y_test, y_pred))
    
    # 학습된 모델을 저장한다.
    with open('pipe.dat', 'wb') as fp :
        pickle.dump(pipe, fp)
        
    print('저장완료')



# step6
# 저장된 모델을 불러와 사용한다.
def step6_using_model() :
    # 객체를 복원한다.
    with open('pipe.dat', 'rb') as fp:
        pipe = pickle.load(fp)

    import numpy as np

    while True :
        text = input('리뷰를 작성해주세요 :')

        str = [text]
        # 예측 정확도
        r1 = np.max(pipe.predict_proba(str) * 100)
        # 예측 결과
        r2 = pipe.predict(str)[0]

        if r2 == '1' :
            print('긍정적인 리뷰')
        else :
            print('부정적인 리뷰')

        print('정확도 : %.3f' % r1)



# 스크래핑 함수
def scrapping() :
    # 각 영화 코드 데이터를 가져와 저장한다.
    # step1_get_detail_url()
    # 140자 평 데이터가 있는 페이지의 주소를 저장한다.
    step2_get_reple_href()
    # 140평 데이터를 가져온다.
    step3_get_reply_data()

# 학습 함수
def learing() :
    text_train, text_test, star_train, star_test = step4_data_preprocessing()
    step5_learning(text_train, star_train, text_test, star_test)

# 사용함수
def using() :
    step6_using_model()


# scrapping()
# learing()
using()



