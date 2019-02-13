import surprise
import pandas as pd

# 기본 제공되는 데이터 사용
def ex1() :
    # 사용할 영화 데이터 셋
    data = surprise.Dataset.load_builtin('ml-100k')
    # print(data)
    # print(data.raw_ratings)
    df = pd.DataFrame(data.raw_ratings, columns=['user', 'item', 'rate', 'ed'])
    print(df)

    # 유사도 계산 방식
    option1 = {'name' : 'msd'}  # 평균 제곱 차이 유사도
    option2 = {'name' : 'cosine'}   # 코사인 유사도
    option3 = {'name' : 'pearson'}  # 피어슨 유사도
    # 추천 목록을 얻기위한 학습 객체 생성
    algo = surprise.KNNBasic(sim_options=option3)
    # 학습 데이터 준비
    trainset = data.build_full_trainset()
    # 학습
    algo.fit(trainset)

    # 196번 사용자에 대해 5개의 영화를 추천한다
    result = algo.get_neighbors(196, k=5)

    for r1 in result :
        print(r1)

# 데이터를 직접 셋팅해서 해본다
def ex2() :
    ratings_expand = {
        '마동석': {
            '택시운전사': 3.5,
            '남한산성': 1.5,
            '킹스맨:골든서클': 3.0,
            '범죄도시': 3.5,
            '아이 캔 스피크': 2.5,
            '꾼': 3.0,
        },
         '이정재': {
            '택시운전사': 5.0,
            '남한산성': 4.5,
            '킹스맨:골든서클': 0.5,
            '범죄도시': 1.5,
            '아이 캔 스피크': 4.5,
            '꾼': 5.0,
        },
        '윤계상': {
            '택시운전사': 3.0,
            '남한산성': 2.5,
            '킹스맨:골든서클': 1.5,
            '범죄도시': 3.0,
            '꾼': 3.0,
            '아이 캔 스피크': 3.5,
        },
        '설경구': {
            '택시운전사': 2.5,
            '남한산성': 3.0,
            '범죄도시': 4.5,
            '꾼': 4.0,
        },
        '최홍만': {
            '남한산성': 4.5,
            '킹스맨:골든서클': 3.0,
            '꾼': 4.5,
            '범죄도시': 3.0,
            '아이 캔 스피크': 2.5,
        },
        '홍수환': {
            '택시운전사': 3.0,
            '남한산성': 4.0,
            '킹스맨:골든서클': 1.0,
            '범죄도시': 3.0,
            '꾼': 3.5,
            '아이 캔 스피크': 2.0,
        },
        '나원탁': {
            '택시운전사': 3.0,
            '남한산성': 4.0,
            '꾼': 3.0,
            '범죄도시': 5.0,
            '아이 캔 스피크': 3.5,
        },
        '소이현': {
            '남한산성': 4.5,
            '아이 캔 스피크': 1.0,
            '범죄도시': 4.0
        }
    }

    # 사용자 목록을 담을 리스트
    name_list = []
    # 영화 목록을 담을 set
    movie_set = set()

    # 사용자 수만큼 반복
    for user_key in ratings_expand :
        # print(user_key)
        name_list.append(user_key)
        # 현재 사용자가 본 영화 목록을 set에 담는다
        for movie_key in ratings_expand[user_key] :
            # print(suer_key, ":", movie_key)
            movie_set.add(movie_key)

    movie_list = list(movie_set)
    print(name_list)
    print(movie_list)

    # 학습할 데이터 준비
    rating_dic = {
        'user_id' : [],
        'item_id' : [],
        'rating' : []
    }

    # 사용자의 수만큼 반복
    for name_key in ratings_expand :
        # 해당 사용자가 본 영화 수만큼 반복
        for movie_key in ratings_expand[name_key] :
            # 사용자 인덱스 번호 추출
            a1 = name_list.index(name_key)
            # 영화 인덱스 번호 추출
            a2 = movie_list.index(movie_key)
            # 평점을 가져온다
            a3 = ratings_expand[name_key][movie_key]
            # 닫는다
            rating_dic['user_id'].append(a1)
            rating_dic['item_id'].append(a2)
            rating_dic['rating'].append(a3)

    # print(rating_dic['user_id'])
    # print(rating_dic['item_id'])
    # print(rating_dic['rating'])
    # 데이터셋을 만든다
    df = pd.DataFrame(rating_dic)
    # 데이터를 읽어들이는 객체 생성
    # rating scale : 평점의 범위
    reader = surprise.Reader(rating_scale=(0.0, 5.0))

    # surprise에서 사용할 데이터셋을 구성할 떄 필요한 이름
    # 데이터가 저장되어 있는 딕셔너리의 컬럼 이름
    # 첫번째 -> user, 두번째 -> item, 세번째 -> rate
    col_list = ['user_id', 'item_id', 'rating']
    data = surprise.Dataset.load_from_df(df[col_list], reader)

    # 학습
    trainset = data.build_full_trainset()
    option = {'name' : 'pearson'}
    algo = surprise.KNNBasic(sim_options=option)
    algo.fit(trainset)

    # 소이현의 영화를 추천받는다
    index = name_list.index('소이현')
    result = algo.get_neighbors(index, k=3)

    for r1 in result :
        print(movie_list[r1-1])

# ex1()
ex2()