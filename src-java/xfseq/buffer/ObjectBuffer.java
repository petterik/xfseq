package xfseq.buffer;

import clojure.lang.*;

/*
 * Keep the primitive IFn entry points available to a type-aware transducer
 * without exposing those interfaces through ObjectBuffer.getInterfaces().
 * The existing analyzer uses the latter to choose a reducing-function shape;
 * reporting this buffer as primitive there would make an ordinary object
 * filter cast arbitrary values to numbers.  The object engine itself still
 * stores every value in the ObjectBuffer.
 */
class ObjectBufferIFn extends AFn implements IFn.OLO, IFn.ODO {

    @Override
    public Object invokePrim(Object a, long b) {
        return invoke(a, Long.valueOf(b));
    }

    @Override
    public Object invokePrim(Object a, double b) {
        return invoke(a, Double.valueOf(b));
    }
}

public class ObjectBuffer extends ObjectBufferIFn implements IXFSeqBuffer {

    private static final Object[] NULLS = new Object[32];
    private static final int MAX_CAPACITY = Integer.MAX_VALUE - 8;

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
            int growth = capacity < 32 ? 4 : 2;
            if (capacity > MAX_CAPACITY / growth) {
                throw new IllegalStateException("ObjectBuffer capacity overflow");
            }
            int nextCapacity = capacity * growth;
            Object[] larger = new Object[nextCapacity];
            System.arraycopy(arr, 0, larger, 0, idx);
            capacity = nextCapacity;
            arr = larger;
        }
        arr[idx++] = b;
        return a;
    }

    @Override
    public ISeq toTail() {
        return toSeq(null);
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
                resetAfterSmallResult();
                break;
            case 2:
                seq = new Cons(arr[0], new Cons(arr[1], seq));
                resetAfterSmallResult();
                break;
            case 3:
                seq = new Cons(arr[0], new Cons(arr[1], new Cons(arr[2], seq)));
                resetAfterSmallResult();
                break;
            case 4:
                seq = new Cons(arr[0], new Cons(arr[1], new Cons(arr[2], new Cons(arr[3], seq))));
                resetAfterSmallResult();
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
                resetAfterExposure();
                break;
            default:
                seq = chunkLargeResult(seq);
                resetAfterExposure();
                break;
        }
        return seq;
    }

    /**
     * Flush the current output as one chunk, even when it contains fewer than
     * five values.  Unary map/filter compatibility uses this shape for a
     * chunked input; generic xf-seq keeps the historical {@link #toSeq} policy.
     */
    public ISeq toChunkSeq(ISeq seq) {
        if (idx == 0) {
            return seq;
        }
        if (idx <= 32) {
            seq = new ChunkedCons(new ArrayChunk(arr, 0, idx), seq);
            resetAfterExposure();
            return seq;
        }
        seq = chunkLargeResult(seq);
        resetAfterExposure();
        return seq;
    }

    private void resetAfterSmallResult() {
        if (capacity > 32) {
            capacity = 32;
            arr = new Object[capacity];
        } else {
            System.arraycopy(NULLS, 0, arr, 0, idx);
        }
        idx = 0;
    }

    private void resetAfterExposure() {
        capacity = Math.min(capacity, 32);
        arr = new Object[capacity];
        idx = 0;
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
