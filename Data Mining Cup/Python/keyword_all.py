import numpy as np
from keyword_util import *

keyword_dict = {}

def set_keyword_dict(filename):
       f = open(filename)
       counter = 1
       for i in f:
              i = i.strip()
              global keyword_dict
              keyword_dict[int(i[2:])] = counter
              counter += 1

def get_train_target(filename):
       f = open(filename)
       t = f.readline().split(',')
       max_l = 0
       r = []
       t = []
       s = []
       for line in f:
       #for i in range(70):
              #line = f.readline()
              line = line.split(',')
              if line[27] and line[27].isalnum():
                     kw = line[6].split('+')
                     kw = filter(None, kw)
                     result = []
                     tar = [0]*5
                     for j in kw:
                            result.append(keyword_dict[int(j[2:])])

                     for j in range(6-len(kw)):
                            result.append(0)
              
                     r.append(result)
                     
                     for i in range(5):
                            tar[i] = line[37+i]

                     t.append(tar)

              kw = line[6].split('+')
              kw = filter(None, kw)
              result = []
              for j in kw:
                     result.append(keyword_dict[int(j[2:])])

              for j in range(6-len(kw)):
                     result.append(0)
              s.append(result)
                     
       return np.array(r, np.int_), np.array(t, np.int_), np.array(s, np.int_)

def get_valid(filename):
       f = open(filename)
       t = f.readline().split(',')
       r = []
       for line in f:
              line = line.split(',')
              kw = line[6].split('+')
              kw = filter(None, kw)
              result = []
              tar = [0]*5
              for j in kw:
                     result.append(keyword_dict[int(j[2:])])

              for j in range(6-len(kw)):
                     result.append(0)
       
              r.append(result)
       return np.array(r, np.int_)

def convert_data(data):
       # 492 keywords
       num_data = data.shape[0]
       r = np.array([]).reshape(0,493)
       for i in range(num_data):
              a = np.array([0] * 493)
              b = np.bincount(data[i])
              if len(a) < len(b):
                  c = b.copy()
                  c[:len(a)] += a
              else:
                  c = a.copy()
                  c[:len(b)] += b

              c = c.reshape(1,-1)
              r = np.concatenate((r, c), axis=0)
       
       return r


if __name__ == '__main__':
       set_keyword_dict('./d/vocabs.txt')
##       #print(keyword_dict)

       t, b, s = get_train_target('./data/SEM_DAILY_BUILD.csv')
       #result = np.array((t,b,0), dtype=object)
       #print(s.shape)
       np.save('./data/KW158175.npy', s)

       #u = get_valid('SEM_DAILY_VALIDATION.csv')
       #np.save('./Data/KWPRD_VALID.npy', u)

       #SaveData('./Data/KEYWORD_GROUPS.npt', t, 'keyword')
       #kw = LoadData('./Data/KEYWORD_GROUPS.npt.npz', 'keyword')
       #kw = convert_data(kw)[:,1:]
##       for i in range(4,40):
##              p, mu, vary, logProbX = mogEM(kw, i, 15)
##              print(i, p)
       
