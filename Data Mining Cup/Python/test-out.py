import nn
import os
import sys
import numpy as np

def test(model, X):
    N = X.shape[0]
    numExPerBat = 100
    batchStart = 0
    Y = None
    while batchStart < N:
        # Batch info
        batchEnd = min(N, batchStart + numExPerBat)
        Ytmp = model.forward(X[batchStart:batchEnd], dropout=False)
        if Y is None:
            Yshape = np.copy(Ytmp.shape)
            Yshape[0] = N
            Y = np.zeros(Yshape)
        Y[batchStart:batchEnd] = Ytmp
        batchStart += numExPerBat
    return Y

if __name__ == '__main__':
    """
    Usage: test.py id -test test.npy
    """
    taskId = sys.argv[1]
    for i in range(2, len(sys.argv)):
        if sys.argv[i] == '-test':
            testDataFile = sys.argv[i + 1]
    testAnswerFile = os.path.join('results/%s' % taskId, '%s.test.o.txt' % taskId)
    testTruthFile = os.path.join('results/%s' % taskId, '%s.test.t.txt' % taskId)
    modelFile = 'results/%s/%s.model.yml' % (taskId, taskId)
    model = nn.load(modelFile)

    model.loadWeights(np.load('results/%s/%s.w.npy' % (taskId, taskId)))
    testData = np.load(testDataFile)

    X = testData
    Y = test(model, X)
    np.savetxt(testAnswerFile,Y, delimiter=',')
