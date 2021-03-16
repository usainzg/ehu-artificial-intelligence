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
        inf = float("inf")

        """
        Paramos si... return True
        - Estamos en => (Estado ganador || Estado perdedor || Limite de profundidad)
        Si no... return False
        """
        def stop(state, cDepth):
            return True if (state.isWin() or state.isLose() or cDepth == self.depth) else False

        def minValue(state, d, ghost):
            if stop(state, d):
                return self.evaluationFunction(state)
            
            v = inf
            for a in state.getLegalActions(ghost):
                if ghost == g_i[-1]: # si es ultimo ghost, siguiente nivel, cambiamos a maxValue()
                    v = min(v, maxValue(state.generateSuccessor(ghost, a), d + 1))
                else: # si no es ultimo ghost, seguimos explorando ese nivel
                    v = min(v, minValue(state.generateSuccessor(ghost, a), d, ghost + 1))
            return v
        
        def maxValue(state, d):
            if stop(state, d):
                return self.evaluationFunction(state)
            
            v = -inf
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
        inf = float("inf")

        def stop(state, cDepth):
            return True if (state.isWin() or state.isLose() or cDepth == self.depth) else False

        def minValue(state, d, ghost, alpha, beta):
            if stop(state, d):
                return self.evaluationFunction(state)
            
            v = inf
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
            if stop(state, d):
                return self.evaluationFunction(state)
            
            v = -inf
            for a in state.getLegalActions(0):
                v = max(v, minValue(state.generateSuccessor(0, a), d, 1, alpha, beta))
            
                if v > beta:
                    return v
            
                alpha = max(alpha, v)
            
            return v

        def alphaBeta(state):
            res = (None, -inf)
            alpha = -inf
            beta = inf
            
            for a in state.getLegalActions(0):
                val = minValue(gameState.generateSuccessor(0, a), 0, 1, alpha, beta)

                if res[1] < val:
                    res = (a, val)
                
                if res[1] > beta:
                    return res[1]
                
                alpha = max(alpha, val)
            
            return res[0]

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
        util.raiseNotDefined()

def betterEvaluationFunction(currentGameState):
    """
    Your extreme ghost-hunting, pellet-nabbing, food-gobbling, unstoppable
    evaluation function (question 5).

    DESCRIPTION: <write something here so we know what you did>
    """
    "*** YOUR CODE HERE ***"
    util.raiseNotDefined()

# Abbreviation
better = betterEvaluationFunction
