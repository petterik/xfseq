package xfseq.buffer;

import clojure.lang.AFn;
import clojure.lang.IFn;
import clojure.lang.ISeq;
import xfseq.LongArrayCons;
import xfseq.LongCons;

public class LongBuffer extends AFn implements IXFSeqBuffer, IFn.OLO {

    private long[] arr;
    private int idx;
    private int capacity = IXFSeqBuffer.MIN_SIZE;

    public LongBuffer() {
        arr = new long[capacity];
    }

    @Override
    public ISeq toTail() {
        return new LongArrayCons(arr, 0, idx, null);
    }

    @Override
    public Object invoke(Object a) {
        return a;
    }

    public Object invoke(Object a, Object b) {
        return invokePrim(a, (Long)b);
    }

    @Override
    public Object invokePrim(Object a, long b) {
        if (idx == capacity) {
            capacity *= capacity < 32 ? 4 : 2;
            long[] larger = new long[capacity];
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
                seq = new LongCons(arr[0], seq);
                idx = 0;
                break;
            case 2:
                seq = new LongCons(arr[0], new LongCons(arr[1], seq));
                idx = 0;
                break;
            case 3:
                seq = new LongCons(arr[0], new LongCons(arr[1], new LongCons(arr[2], seq)));
                idx = 0;
                break;
            case 4:
                seq = new LongCons(arr[0], new LongCons(arr[1], new LongCons(arr[2], new LongCons(arr[3], seq))));
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
                seq = new LongArrayCons(arr, 0, idx, seq);
                arr = new long[capacity];
                idx = 0;
                break;
            default:
                seq = chunkLargeResult(seq);
                capacity = 32;
                arr = new long[capacity];
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
            s = new LongArrayCons(arr, offset, end, s);
        } while (offset > 0);

        return s;
    }
}