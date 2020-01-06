package xfseq.buffer;

import clojure.lang.IFn;
import clojure.lang.ISeq;
import xfseq.DoubleArrayCons;
import xfseq.DoubleCons;

public class DoubleBuffer implements IXFSeqBuffer, IFn.ODO {

    private double[] arr;
    private int idx;
    private int capacity = IXFSeqBuffer.MIN_SIZE;

    public DoubleBuffer() {
        arr = new double[capacity];
    }

    @Override
    public ISeq toTail() {
        return new DoubleArrayCons(arr, 0, idx, null);
    }

    @Override
    public Object invoke(Object a) {
        return null;
    }

    public Object invoke(Object a, Object b) {
        return invokePrim(a, (Long)b);
    }

    @Override
    public Object invokePrim(Object a, double b) {
        if (idx == capacity) {
            capacity *= capacity < 32 ? 4 : 2;
            double[] larger = new double[capacity];
            System.arraycopy(arr, 0, larger, 0, idx);
            arr = larger;
        }
        arr[idx++] = b;
        return a;
    }

    @Override
    public boolean isEmpty() {
        return idx == 0;
    }

    public ISeq toSeq(ISeq seq) {
        switch (idx) {
            case 0:
                break;
            // TODO: Verify whether handrolling some cases is a good idea.
            case 1:
                seq = new DoubleCons(arr[0], seq);
                idx = 0;
                break;
            case 2:
                seq = new DoubleCons(arr[0], new DoubleCons(arr[1], seq));
                idx = 0;
                break;
            case 3:
                seq = new DoubleCons(arr[0], new DoubleCons(arr[1], new DoubleCons(arr[2], seq)));
                idx = 0;
                break;
            case 4:
                seq = new DoubleCons(arr[0], new DoubleCons(arr[1], new DoubleCons(arr[2], new DoubleCons(arr[3], seq))));
                idx = 0;
                break;
            case 5:
            case 6:
            case 7:
            case 8:
            case 9:
            case 10:
            case 11:
            case 12:
            case 13:
            case 14:
            case 15:
            case 16:
            case 17:
            case 18:
            case 19:
            case 20:
            case 21:
            case 22:
            case 23:
            case 24:
            case 25:
            case 26:
            case 27:
            case 28:
            case 29:
            case 30:
            case 31:
            case 32:
                seq = new DoubleArrayCons(arr, 0, idx, seq);
                arr = new double[capacity];
                idx = 0;
                break;
            default:
                seq = chunkLargeResult(seq);
                capacity = 32;
                arr = new double[capacity];
                idx = 0;
                break;
        }
        return seq;
    }

    private ISeq chunkLargeResult(ISeq s) {
        // Returns 32 sized chunks in case the transduction created
        // _more items than that. When chained, it can blow up.
        // This problem was found with code that repeated interpose:
        // (interpose nil (interpose nil ... (interpose nil (range)) ... ))
        int offset = idx;
        do {
            int end = offset;
            offset = Math.max(0, offset - 32);
            s = new DoubleArrayCons(arr, offset, end, s);
        } while (offset > 0);

        return s;
    }
}
