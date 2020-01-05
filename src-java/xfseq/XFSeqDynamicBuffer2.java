package xfseq;

import clojure.lang.*;

public final class XFSeqDynamicBuffer2 extends AFn {

    // Note: Changing this from 2, 4 and 8, sees huge difference
    //       in performance (java8).
    //       Setting it to 8 makes it faster on map inc and filter even
    //       compared to vanilla lazy-seqs
    private static final int MIN_SIZE = 8;

    private Object[] arr;
    private int idx;

    public XFSeqDynamicBuffer2(){
        idx = 0;
        arr = new Object[MIN_SIZE];
    }

    public void scope() {
        if (arr == null) {
            arr = new Object[idx < MIN_SIZE ? MIN_SIZE : idx];
        }
    }

    public void scope(int size) {
        if (arr == null) {
            arr = new Object[size < MIN_SIZE ? MIN_SIZE : size];
        }
    }

    public ISeq toSeq() {
        if (idx == 0) {
            return null;
        } else {
            // Doesn't need to set arr to nil as this is the final step in XFSeq.NextStep
            return new ChunkedCons(new ArrayChunk(arr, 0, idx), null);
        }
    }

    public Object toSeq(ISeq more) {
        Object s;
        switch(idx) {
            case 0:
                s = more;
                break;
            // TODO: Verify whether handrolling these arities is a good idea.
            case 1:
                s = new Cons(arr[0], more);
                idx = 0;
                arr[0] = null;
                break;
            case 2:
                s = new Cons(arr[0], new Cons(arr[1], more));
                idx = 0;
                arr[0] = null;
                arr[1] = null;
                break;
            case 3:
                s = new Cons(arr[0], new Cons(arr[1], new Cons(arr[2], more)));
                idx = 0;
                arr[0] = null;
                arr[1] = null;
                arr[2] = null;
                break;
            case 4:
                s = new Cons(arr[0], new Cons(arr[1], new Cons(arr[2], new Cons(arr[3], more))));
                idx = 0;
                arr[0] = null;
                arr[1] = null;
                arr[2] = null;
                arr[3] = null;
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
                s = new ChunkedCons(new ArrayChunk(arr, 0, idx), more);
                scope(idx);
                idx = 0;
                break;
            default:
                // Returns 32 sized chunks in case the transduction created
                // more items than that. When chained, it can blow up.
                // This problem was found with code that repeated interpose:
                // (interpose nil (interpose nil ... (interpose nil (range)) ... ))
                s = more;
                int offset = idx;
                do {
                    int end = offset;
                    offset = Math.max(0, offset - 32);
                    s = new ChunkedCons(new ArrayChunk(arr, offset, end), (ISeq)s);
                } while (offset > 0);
                scope(32);
                idx = 0;
                break;
        }
        return s;
    }

    // Implements a reducing function (arities: 0, 1, 2)
    public Object invoke() {
        return new XFSeqDynamicBuffer2();
    }

    public Object invoke(Object a) {
        return this;
    }

    public Object invoke(Object a, Object b) {
        // assert(a == this);
        if (idx == arr.length) {
            // Grows quickly to 32, then slows down.
            // 8 * 4 * 2 * 2 * 2
            Object[] larger = new Object[idx * (idx <= 8 ? 4 : 2)];
            System.arraycopy(arr, 0, larger, 0, idx);
            arr = larger;
        }

        arr[idx++] = b;
        return this;
    }
}
