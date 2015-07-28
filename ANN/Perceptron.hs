module Perceptron where

type WeightVector = [Float]
type InputVector  = [Float]
data TrainingSet = TrainingSet (InputVector, Float)
data Perceptron  = Perceptron WeightVector deriving(Show)

getTrainingInput :: TrainingSet->InputVector
getTrainingInput (TrainingSet (ti, _)) = ti

getExpectedValue :: TrainingSet->Float
getExpectedValue (TrainingSet (_, value)) = value

getWeights :: Perceptron->WeightVector
getWeights (Perceptron w) = w

getOutput :: Perceptron->InputVector->Float
getOutput p = stepFunction (getWeights p)

stepFunction :: WeightVector->InputVector->Float
stepFunction w x = if dotProduct w x > 1.0 then 1.0 else 0.0

dotProduct :: [Float]->[Float]->Float
dotProduct a b = sum (zipWith (*) a b)

--Return a weight vector/list after modyfing it with a single training set
modifyWeights :: WeightVector->TrainingSet->WeightVector
modifyWeights w ts = zipWith (+) w modifierV
              where modifierV = map (* (0.3 * (getExpectedValue ts - stepFunction w (getTrainingInput ts)))) (getTrainingInput ts)

--Return a perceptron after training the input one with a list of training sets
trainPerceptron :: Perceptron->[TrainingSet]->Perceptron
trainPerceptron p [] = p
trainPerceptron p (x:xs) = trainPerceptron newPerceptron xs
                where newPerceptron = Perceptron (modifyWeights (getWeights p) x)
