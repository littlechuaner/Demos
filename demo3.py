import re  #正则表达式
from bs4 import BeautifulSoup  #html标签处理
import pandas as pd

def review_to_wordlist(review):
    '''
    把IMDB的评论转成词序列
    '''
    # 去掉HTML标签，拿到内容
    review_text = BeautifulSoup(review).get_text()
    # 用正则表达式取出符合规范的部分
    review_text = re.sub("[^a-zA-Z]"," ", review_text)
    # 小写化所有的词，并转成词list
    words = review_text.lower().split()
    # 返回words
    return words

# 使用pandas读入训练和测试csv文件
train = pd.read_csv('/Users/Hanxiaoyang/IMDB_sentiment_analysis_data/labeledTrainData.tsv', header=0, delimiter="\t", quoting=3)
test = pd.read_csv('/Users/Hanxiaoyang/IMDB_sentiment_analysis_data/testData.tsv', header=0, delimiter="\t", quoting=3 )
# 取出情感标签，positive/褒 或者 negative/贬
y_train = train['sentiment']
# 将训练和测试数据都转成词list
train_data = []
for i in xrange(0,len(train['review'])):
    train_data.append(" ".join(review_to_wordlist(train['review'][i])))
test_data = []
for i in xrange(0,len(test['review'])):
    test_data.append(" ".join(review_to_wordlist(test['review'][i])))

from sklearn.feature_extraction.text import TfidfVectorizer as TFIV
# 初始化TFIV对象，去停用词，加2元语言模型
tfv = TFIV(min_df=3,  max_features=None, strip_accents='unicode', analyzer='word',token_pattern=r'\w{1,}', ngram_range=(1, 2), use_idf=1,smooth_idf=1,sublinear_tf=1, stop_words = 'english')
# 合并训练和测试集以便进行TFIDF向量化操作
X_all = train_data + test_data
len_train = len(train_data)

# 这一步有点慢，去喝杯茶刷会儿微博知乎歇会儿...
tfv.fit(X_all)
X_all = tfv.transform(X_all)
# 恢复成训练集和测试集部分
X = X_all[:len_train] 
X_test = X_all[len_train:]