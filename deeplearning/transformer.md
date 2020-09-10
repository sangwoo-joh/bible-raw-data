# Transformer

## Why ReLu works better?
 - Hinton: Allows neuron to express a strong opinion
 - Gradient doesn't saturate (on the high end)
 - Less sensitive to random initialization
   - e.g. for `tanh` or sigmoid, we need to be careful about the initial value so that it could be in-between the range of high gradient
 - Runs great on low precision hardware
   - e.g. it just increase for x > 0
