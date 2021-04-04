# mira.py
# -------
# Licensing Information:  You are free to use or extend these projects for
# educational purposes provided that (1) you do not distribute or publish
# solutions, (2) you retain this notice, and (3) you provide clear
# attribution to UC Berkeley, including a link to http://ai.berkeley.edu.
# 
# Attribution Information: The Pacman AI projects were developed at UC Berkeley.
# The core projects and autograders were primarily created by John DeNero
# (denero@cs.berkeley.edu) and Dan Klein (klein@cs.berkeley.edu).
# Student side autograding was added by Brad Miller, Nick Hay, and
# Pieter Abbeel (pabbeel@cs.berkeley.edu).


# Mira implementation
import util
PRINT = True
inf = float('inf')

class MiraClassifier:
    """
    Mira classifier.

    Note that the variable 'datum' in this code refers to a counter of features
    (not to a raw samples.Datum).
    """
    def __init__( self, legalLabels, max_iterations):
        self.legalLabels = legalLabels
        self.type = "mira"
        self.automaticTuning = False
        self.C = 0.001
        self.legalLabels = legalLabels
        self.max_iterations = max_iterations
        self.initializeWeightsToZero()

    def initializeWeightsToZero(self):
        "Resets the weights of each label to zero vectors"
        self.weights = {}
        for label in self.legalLabels:
            self.weights[label] = util.Counter() # this is the data-structure you should use

    def train(self, trainingData, trainingLabels, validationData, validationLabels):
        "Outside shell to call your method. Do not modify this method."

        self.features = trainingData[0].keys() # this could be useful for your code later...

        if (self.automaticTuning):
            Cgrid = [0.002, 0.004, 0.008]
        else:
            Cgrid = [self.C]

        return self.trainAndTune(trainingData, trainingLabels, validationData, validationLabels, Cgrid)

    def trainAndTune(self, trainingData, trainingLabels, validationData, validationLabels, Cgrid):
        """
        This method sets self.weights using MIRA.  Train the classifier for each value of C in Cgrid,
        then store the weights that give the best accuracy on the validationData.

        Use the provided self.weights[label] data structure so that
        the classify method works correctly. Also, recall that a
        datum is a counter from features to values for those features
        representing a vector of values.
        """
        "*** YOUR CODE HERE ***"
        weights = {}
        def train_c(c):
            '''
            Restablecemos los pesos
            '''
            self.weights = dict((label, util.Counter()) for label in self.legalLabels)
            
            '''
            Realizamos tantas iteraciones como este especificado en self.max_iterations.
            '''
            for i in range(self.max_iterations):
                '''
                Para cada features (x) y labels (y)...
                '''
                for x, y in zip(trainingData, trainingLabels):
                    y_p = self.classify([x])[0]
                    '''
                    Si el label devuelto por self.classify() no es el mismo que el label
                    real, (y)... actualizar.
                    '''
                    if y != y_p:
                        t = ((self.weights[y_p] - self.weights[y]) * x + 1.0) / (2.0 * (x * x))
                        tau = min([c, t])
                        '''
                        Actualizamos los pesos:
                        -> wY = wY + tF
                        -> wY' = wY' - tF
                        Donde t:
                        -> t = min(c, tAux)
                        -> tAux = ((wY' - wY) * f + 1.0) / (2.0 * (f * f))
                        '''
                        x_copy = x.copy()
                        for key, value in x_copy.items():
                            x_copy[key] = value * tau # (tF)
                        
                        self.weights[y] += x_copy
                        self.weights[y_p] -= x_copy

            weights[c] = self.weights
            res = 0
            for y, y_p in zip(validationLabels, self.classify(validationData)):
                if y == y_p:
                    res += 1 # Si bien clasificado... sumar
            return res # Devolver numero de bien clasificados
        
        
        '''
        Ejecutamos train_c() con todos los c en Cgrid.
        '''
        scores = [train_c(c) for c in Cgrid]

        '''
        Tenemos que elegir cual es el mejor c, es decir, el que mejores resultados no ha dado,
        o en caso de empate, el c menor.
        '''
        c_aux, aux_score = Cgrid[0], -inf
        for c, c_score in zip(Cgrid, scores):
            if (c_score > aux_score) or (c_score == aux_score and c < c_aux):
                c_aux, aux_score = c, c_score

        self.weights = weights[c_aux] # Pesos con el mejor c -> Mejores pesos
        self.C = c_aux
        return c_aux


    def classify(self, data ):
        """
        Classifies each datum as the label that most closely matches the prototype vector
        for that label.  See the project description for details.

        Recall that a datum is a util.counter...
        """
        guesses = []
        for datum in data:
            vectors = util.Counter()
            for l in self.legalLabels:
                vectors[l] = self.weights[l] * datum
            guesses.append(vectors.argMax())
        return guesses


