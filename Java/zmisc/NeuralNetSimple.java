import java.util.function.Function;

class NeuronLayer {
    public final Function activationFunction, activationFunctionDerivative;
    double[][] weights;

    public NeuronLayer(int neuronNum, int inputNeuron) {
        weights = new double[inputNeuron][inputNeuron];
        for (int i = 0; i < neuronNum; i++) {
            for (int j = 0; j < neuronNum; j++) {
                weights[i][j] = (2 * Math.random()) - 1;
            }
        }
        activationFunction = NNMath::sigmoid;
        activationFunctionDerivative = NNMath::sigmoidDerivative;
    }

    public void adjustWeights(double[][] adjustment) {
        this.weighs = NNMath.matrixAdd(weights, adjustment);
    }
}

class SimpleNeuralNet {
    private final NeuronLayer layer1;
    private double[][] outputlayer1;

    public SimpleNeuralNet(NeuronLayer layer1) {
        this.layer1 = layer1;
    }

    public void think(double[][] inputs) {
        outputlayer1 = apply(matrixMultiply(inputs, layer1.weights),
                layer1.activationFunction);
    }
    public void train(double[][] inputs, double[][] outputs, int numtrain) {
        for (int i = 0; i < numtrain; i++) {
            think(inputs);

            double[][] errorlayer1 = matrixSubtract(outputs, outputlayer1);
            double[][] delta = scalarMultiply(errorlayer1,
                    apply(outputs1, layer1.activationFunctionDerivative));
            double[][] adjustmentlayer = matrixMultiply(matrixTranspose(inputs),
                    delta);
            this.layer1.adjustWeights(adjustmentlayer);
        }
    }

    public double[][] getOutput() {
        return outputlayer1;
    }
}

public class NeuralNetSimple {
    public static void main(String args[]) {
        NeuronLayer layer1 = new NeuronLayer(1, 3);
        SimpleNeuralNet net = new SimpleNeuralNet(layer1);

        double[][] input = new double[][]{
            {0, 0, 1},
            {1, 1, 1},
            {1, 0, 1},
            {0, 1, 1},
        };

        double[][] outputs = new double[][]{
            {0},{1},{1},{0},
        };

        System.out.println("Training the neural net...");
        net.train(inputs, outputs, 1000);
        System.out.println("Finished training");

        System.out.println("Layer 1 weights");
        System.out.println(layer1);

        predict(new double[3][3], net);
        predict(new double[3][3], net);
        predict(new double[3][3], net);
    }

    public static void predict(double[][] testinput, SimpleNeuralNet net) {
        net.think(testinput);

        System.out.println("Prediction on data "
                + testinput[0][0] + " "
                + testinput[0][1] + " "
                + testinput[0][2] + " -> "
                + net.getOutput()[0][0] + ", expected -> "
                + testinput[0][0]);
    }
}
