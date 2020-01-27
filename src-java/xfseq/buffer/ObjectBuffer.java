package xfseq.buffer;

import clojure.lang.*;

public class ObjectBuffer extends AFn implements IXFSeqBuffer {

    private static final Object[] NULLS = new Object[32];

    private Object[] arr;
    private int idx;
    private int capacity = IXFSeqBuffer.MIN_SIZE;

    public ObjectBuffer() {
        arr = new Object[capacity];
    }

    @Override
    public Object invoke(Object a) {
        return a;
    }

    @Override
    public Object invoke(Object a, Object b) {
        if (idx == capacity) {
            capacity *= capacity < 32 ? 4 : 2;
            Object[] larger = new Object[capacity];
            System.arraycopy(arr, 0, larger, 0, idx);
            arr = larger;
        }
        arr[idx++] = b;
        return a;
    }

    @Override
    public ISeq toTail() {
        return new ChunkedCons(new ArrayChunk(arr, 0, idx), null);
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
                seq = new Cons(arr[0], seq);
                arr[0] = null;
                idx = 0;
                break;
            case 2:
                seq = new Cons(arr[0], new Cons(arr[1], seq));
                System.arraycopy(NULLS, 0, arr, 0, idx);
                idx = 0;
                break;
            case 3:
                seq = new Cons(arr[0], new Cons(arr[1], new Cons(arr[2], seq)));
                System.arraycopy(NULLS, 0, arr, 0, idx);
                idx = 0;
                break;
            case 4:
                seq = new Cons(arr[0], new Cons(arr[1], new Cons(arr[2], new Cons(arr[3], seq))));
                System.arraycopy(NULLS, 0, arr, 0, idx);
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
                seq = new ChunkedCons(new ArrayChunk(arr, 0, idx), seq);
                arr = new Object[capacity];
                idx = 0;
                break;
            default:
                seq = chunkLargeResult(seq);
                capacity = 32;
                arr = new Object[capacity];
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
            s = new ChunkedCons(new ArrayChunk(arr, offset, end), s);
        } while (offset > 0);

        return s;
    }
}
