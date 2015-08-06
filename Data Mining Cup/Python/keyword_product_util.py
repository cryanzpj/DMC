import numpy as np

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
       return np.array(r, np.int_), np.array(t, np.int_)


if __name__ == '__main__':
       set_keyword_dict('d/vocabs.txt')
##       #print(keyword_dict)

       t, b = get_train_target('data/SEM_DAILY_BUILD.csv')
       result = np.array((t,b,0), dtype=object)
       np.save('train-prd.npy', result)
