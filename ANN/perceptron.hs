module Perceptron where

data TrainingSet = TrainingSet ([Float], Float)
data Perceptron  = Perceptron [Float] deriving(Show)

getTrainingInput :: TrainingSet->[Float]
getTrainingInput (TrainingSet (ti, _)) = ti

getExpectedValue :: TrainingSet->Float
getExpectedValue (TrainingSet (_, value)) = value

getWeights :: Perceptron->[Float]
getWeights (Perceptron w) = w

getOutput :: Perceptron->[Float]->Float
getOutput p = stepFunction (getWeights p)

stepFunction :: [Float]->[Float]->Float
stepFunction w x = if dotProduct w x > 1.0 then 1.0 else 0.0

dotProduct :: [Float]->[Float]->Float
dotProduct v1 v2 = sum (zipWith (*) v1 v2)

--Return a weight vector/list after modyfing it with a single training set
modifyWeights :: [Float]->TrainingSet->[Float]
modifyWeights w ts = zipWith (+) w modifierV
              where modifierV = map (* (0.3 * (getExpectedValue ts - stepFunction w (getTrainingInput ts)))) (getTrainingInput ts)

--Return a perceptron after training the input one with a list of training sets
trainPerceptron :: Perceptron->[TrainingSet]->Perceptron
trainPerceptron p [] = p
trainPerceptron p (x:xs) = trainPerceptron newPerceptron xs
                where newPerceptron = Perceptron (modifyWeights (getWeights p) x)

trainingSets :: [TrainingSet]
trainingSets = cycle (map TrainingSet [ ([1,1], 1), ([1,0], 0), ([0,1], 0), ([0,0],0)])

andPerceptron :: Perceptron
andPerceptron = Perceptron [0.1,0.1]

trainedAndPerceptron :: Perceptron
trainedAndPerceptron = trainPerceptron andPerceptron (take 100 trainingSets)

main :: IO ()
main = do
        print (map (getOutput trainedAndPerceptron) [ [1.0, 1.0], [1.0, 0.0], [0.0, 0.0], [0.0, 1.0]])
