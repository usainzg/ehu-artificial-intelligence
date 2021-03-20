# multiAgents.py
# --------------
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


from util import manhattanDistance
from game import Directions
import random, util

from game import Agent

inf = float("inf")

class ReflexAgent(Agent):
    """
    A reflex agent chooses an action at each choice point by examining
    its alternatives via a state evaluation function.

    The code below is provided as a guide.  You are welcome to change
    it in any way you see fit, so long as you don't touch our method
    headers.
    """
    def getAction(self, gameState):
        """
        You do not need to change this method, but you're welcome to.

        getAction chooses among the best options according to the evaluation function.

        Just like in the previous project, getAction takes a GameState and returns
        some Directions.X for some X in the set {NORTH, SOUTH, WEST, EAST, STOP}
        """
        # Collect legal moves and successor states
        legalMoves = gameState.getLegalActions()

        # Choose one of the best actions
        scores = [self.evaluationFunction(gameState, action) for action in legalMoves]
        bestScore = max(scores)
        bestIndices = [index for index in range(len(scores)) if scores[index] == bestScore]
        chosenIndex = random.choice(bestIndices) # Pick randomly among the best

        "Add more of your code here if you want to"

        return legalMoves[chosenIndex]

    def evaluationFunction(self, currentGameState, action):
        """
        Design a better evaluation function here.

        The evaluation function takes in the current and proposed successor
        GameStates (pacman.py) and returns a number, where higher numbers are better.

        The code below extracts some useful information from the state, like the
        remaining food (newFood) and Pacman position after moving (newPos).
        newScaredTimes holds the number of moves that each ghost will remain
        scared because of Pacman having eaten a power pellet.

        Print out these variables to see what you're getting, then combine them
        to create a masterful evaluation function.
        """
        # Useful information you can extract from a GameState (pacman.py)
        successorGameState = currentGameState.generatePacmanSuccessor(action)
        newPos = successorGameState.getPacmanPosition()
        newFood = successorGameState.getFood().asList()
        newGhostStates = successorGameState.getGhostStates()
        newScaredTimes = [ghostState.scaredTimer for ghostState in newGhostStates]

        "*** YOUR CODE HERE ***"
        ghost_pos = [(G.getPosition()[0], G.getPosition()[1]) for G in newGhostStates]
        new_scared = min(newScaredTimes) > 0
        cur_food_list = currentGameState.getFood().asList()

        """
        Si no hay nuevos ScaredTimes => nuevo estado es fantasma, -1.0
        """
        if not new_scared and (newPos in ghost_pos):
            return -1.0

        if newPos in cur_food_list:
            return 1.0
        
        closest_food = 99999999
        for f in newFood:
            d = util.manhattanDistance(newPos, f)
            if d < closest_food:
                closest_food = d

        closest_ghost = 99999999
        for g in ghost_pos:
            d = util.manhattanDistance(newPos, g)
            if d < closest_ghost:
                closest_ghost = d
        
        """
        - Cuanto mas CERCA esta la comida => numero mayor.
        - Cuanto mas LEJOS el fantasma => numero menor.
        - Se busca maximizar la resta, por lo tanto, 
        si al comida cerca y si el fantasma lejos... mejor puntuacion.
        """
        return 1.0 / closest_food - 1.0 / closest_ghost


def scoreEvaluationFunction(currentGameState):
    """
    This default evaluation function just returns the score of the state.
    The score is the same one displayed in the Pacman GUI.

    This evaluation function is meant for use with adversarial search agents
    (not reflex agents).
    """
    return currentGameState.getScore()

class MultiAgentSearchAgent(Agent):
    """
    This class provides some common elements to all of your
    multi-agent searchers.  Any methods defined here will be available
    to the MinimaxPacmanAgent, AlphaBetaPacmanAgent & ExpectimaxPacmanAgent.

    You *do not* need to make any changes here, but you can if you want to
    add functionality to all your adversarial search agents.  Please do not
    remove anything, however.

    Note: this is an abstract class: one that should not be instantiated.  It's
    only partially specified, and designed to be extended.  Agent (game.py)
    is another abstract class.
    """

    def __init__(self, evalFn = 'scoreEvaluationFunction', depth = '2'):
        self.index = 0 # Pacman is always agent index 0
        self.evaluationFunction = util.lookup(evalFn, globals())
        self.depth = int(depth)
    
    def stop(self, state, cDepth):
            return True if (cDepth == self.depth or state.isWin() or state.isLose()) else False

class MinimaxAgent(MultiAgentSearchAgent):
    """
    Your minimax agent (question 2)
    """
    def getAction(self, gameState):
        """
        Returns the minimax action from the current gameState using self.depth
        and self.evaluationFunction.

        Here are some method calls that might be useful when implementing minimax.

        gameState.getLegalActions(agentIndex):
        Returns a list of legal actions for an agent
        agentIndex=0 means Pacman, ghosts are >= 1

        gameState.generateSuccessor(agentIndex, action):
        Returns the successor game state after an agent takes an action

        gameState.getNumAgents():
        Returns the total number of agents in the game

        gameState.isWin():
        Returns whether or not the game state is a winning state

        gameState.isLose():
        Returns whether or not the game state is a losing state
        """
        "*** YOUR CODE HERE ***"
        g_i = [i for i in range(1, gameState.getNumAgents())]

        """
        Paramos si... return True
        - Estamos en => (Estado ganador || Estado perdedor || Limite de profundidad)
        Si no... return False
        """
        def minValue(state, d, ghost):
            v = inf

            if self.stop(state, d):
                return self.evaluationFunction(state)
            
            for a in state.getLegalActions(ghost):
                if ghost == g_i[-1]: # si es ultimo ghost, siguiente nivel, cambiamos a maxValue()
                    v = min(v, maxValue(state.generateSuccessor(ghost, a), d + 1))
                else: # si no es ultimo ghost, seguimos explorando ese nivel
                    v = min(v, minValue(state.generateSuccessor(ghost, a), d, ghost + 1))
            return v
        
        def maxValue(state, d):
            v = -inf

            if self.stop(state, d):
                return self.evaluationFunction(state)
            
            for a in state.getLegalActions(0):
                v = max(v, minValue(state.generateSuccessor(0, a), d, 1))
            return v

        """
        Por cada accion legal... rellenamos con duplas (accion, val_minValue).
        - val_minValue realiza la llamada recursiva.
        """
        sol = [(a, minValue(gameState.generateSuccessor(0, a), 0, 1)) for a in gameState.getLegalActions(0)]
        sol = sorted(sol, key=lambda k: k[1], reverse=True) # ordenamos por el valor
        # Aqui sol contiene la (accion, valor) con mayor valor como primer elemento
        return sol[0][0] # devolvemos la primera accion => accion con mayor valor

class AlphaBetaAgent(MultiAgentSearchAgent):
    """
    Your minimax agent with alpha-beta pruning (question 3)
    """
    def getAction(self, gameState):
        """
        Returns the minimax action using self.depth and self.evaluationFunction
        """
        "*** YOUR CODE HERE ***"
        g_i = [i for i in range(1, gameState.getNumAgents())]

        def minValue(state, d, ghost, alpha, beta):
            v = inf

            if self.stop(state, d):
                return self.evaluationFunction(state)
            
            for a in state.getLegalActions(ghost):
                if ghost == g_i[-1]: # si es ultimo ghost, siguiente nivel, cambiamos a maxValue()
                    v = min(v, maxValue(state.generateSuccessor(ghost, a), d + 1, alpha, beta))
                else: # si no es ultimo ghost, seguimos explorando ese nivel
                    v = min(v, minValue(state.generateSuccessor(ghost, a), d, ghost + 1, alpha, beta))
            
                if v < alpha:
                    return v
            
                beta = min(beta, v)
            return v
        
        def maxValue(state, d, alpha, beta):
            v = -inf

            if self.stop(state, d):
                return self.evaluationFunction(state)
            
            for a in state.getLegalActions(0):
                v = max(v, minValue(state.generateSuccessor(0, a), d, 1, alpha, beta))
            
                if v > beta:
                    return v
            
                alpha = max(alpha, v)
            return v

        def alphaBeta(state):
            res = (None, -inf) # Accion, valor
            alpha = -inf
            beta = inf
            
            for a in state.getLegalActions(0):
                val = minValue(gameState.generateSuccessor(0, a), 0, 1, alpha, beta)

                if res[1] < val:
                    res = (a, val) # actualizar con (accion, valor)
                
                if res[1] > beta:
                    return res[1] # devolver valor
                
                alpha = max(alpha, val)
            return res[0] # devolver accion

        def pruningAlphaBeta(state):
            return alphaBeta(state)
        
        return pruningAlphaBeta(gameState)

class ExpectimaxAgent(MultiAgentSearchAgent):
    """
      Your expectimax agent (question 4)
    """
    def getAction(self, gameState):
        """
        Returns the expectimax action using self.depth and self.evaluationFunction

        All ghosts should be modeled as choosing uniformly at random from their
        legal moves.
        """
        "*** YOUR CODE HERE ***"
        g_i = [i for i in range(1, gameState.getNumAgents())]

        def maxValue(state, d):
            v = -inf
            if self.stop(state, d):
                return self.evaluationFunction(state)
            
            for a in state.getLegalActions(0):
                v = max(v, expectedValue(state.generateSuccessor(0, a), d, 1))
            return v
        
        def expectedValue(state, d, ghost):
            v = 0

            if self.stop(state, d):
                return self.evaluationFunction(state)
            
            n = len(state.getLegalActions(ghost))
            prob = 1 / n # probabilidad para calcular el valor del expected value

            for a in state.getLegalActions(ghost):
                if ghost == g_i[-1]:
                    v += prob * maxValue(state.generateSuccessor(ghost, a), d + 1)
                else:
                    v += prob * expectedValue(state.generateSuccessor(ghost, a), d, ghost + 1)
            return v
        
        sol = [(a, expectedValue(gameState.generateSuccessor(0, a), 0, 1)) for a in gameState.getLegalActions(0)]
        sol = sorted(sol, key=lambda k: k[1], reverse=True) # ordenamos por el valor
        # Aqui sol contiene la (accion, valor) con mayor valor como primer elemento
        return sol[0][0] # devolvemos la primera accion => accion con mayor valor

def betterEvaluationFunction(currentGameState):
    """
    Your extreme ghost-hunting, pellet-nabbing, food-gobbling, unstoppable
    evaluation function (question 5).

    DESCRIPTION:
    - Pesos de factores:
        - Puntuacion actual: 1
        - Numero de comidas restantes: 15
        - Puntuacion fantasmas: 1
        - Capsulas restantes: 2
        - Comida mas cercana: 1.5
    - Funcionamiento:
        - Puntuacion fantasmas:
            - Para cada fantasma:
                - Calcular la distancia de la posicion del pacman
                al fantasma.
                - Obtener el scaredTimer del fanstama (movimiento restantes como asustado).
                - Si no asustado (sc == 0), actualizamos la distancia minima (closest...) anterior si
                la distancia actual es menor.
                - Si sc > d, es decir, le queda mas movimientos como asustado que la distancia al
                fantasma, puntuar con (150 - d).
                - En otro caso, puntuar pero menos (40 - d) => mejora?
            - Actualizar la distancia minima a fantasmas (closest...), si seguimos con el inf (valor inicial),
            lo ponemos a 0, si no, dejamos el valor actual (minimo).
            - Sumamos a la puntuacion anterior la distancia minima.
        - Puntuacion comida mas cercana:
            - Ordenamos el array de posiciones de la comida en base a la distancia de cada elemento,
            es decir, se ordena en base a la distancia (manhattan). El primer elemento sera el que 
            tenga una distancia menor.
            - Si hay comidas restantes, calculamos la distancia y lo guardamos en la variable closest_food...
                
    """
    "*** YOUR CODE HERE ***"
    new_pos = currentGameState.getPacmanPosition()
    new_ghost_states = currentGameState.getGhostStates()
    new_food = currentGameState.getFood().asList()
    num_food = len(new_food)
    new_capsules = currentGameState.getCapsules()
    cur_score = currentGameState.getScore()
    
    closest_ghost_distance = inf
    ghost_score = 0

    for g in new_ghost_states:
        sc = g.scaredTimer
        d = manhattanDistance(g.getPosition(), new_pos)
        if sc == 0:
            if d < closest_ghost_distance:
                closest_ghost_distance = d
        elif sc > d:
            ghost_score += 150 - d
        else:
            ghost_score += 40 - d # mejora?
    
    closest_ghost_distance = 0 if closest_ghost_distance == inf else closest_ghost_distance
    ghost_score += closest_ghost_distance
    
    new_food_sorted = sorted(new_food, key=lambda p: manhattanDistance(p, new_pos))
    closest_food_distance = manhattanDistance(new_food_sorted[0], new_pos) if len(new_food_sorted) > 0 else 0

    return cur_score - 15 * num_food + ghost_score + 2 * len(new_capsules) - 1.5 * closest_food_distance

# Abbreviation
better = betterEvaluationFunction
