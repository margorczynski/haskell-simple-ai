module Main where

import Perceptron

trainingSets :: [TrainingSet]
trainingSets = cycle (map TrainingSet [ ([1,1], 1), ([1,0], 0), ([0,1], 0), ([0,0],0)])

andPerceptron :: Perceptron
andPerceptron = Perceptron [0.1,0.1]

trainedAndPerceptron :: Perceptron
trainedAndPerceptron = trainPerceptron andPerceptron (take 100 trainingSets)

main :: IO ()
main = do
        print (map (getOutput trainedAndPerceptron) [ [1.0, 1.0], [1.0, 0.0], [0.0, 0.0], [0.0, 1.0]])
