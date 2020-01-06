package xfseq;

// TODO: Something went wrong when refactoring from InlineStep.

import clojure.lang.*;
import xfseq.buffer.IXFSeqBuffer;

public abstract class XFSeqStep extends AFn {

    private ISeq s;
    final IXFSeqBuffer buf;

    public XFSeqStep(ISeq s, IXFSeqBuffer buf) {
        this.s = s;
        this.buf = buf;
    }

    abstract void invokeXF();
    abstract boolean invokeXF(ISeq s);
    abstract boolean invokeXFChunk(IChunk cs);

    public Object invoke() {
        for(ISeq c = this.s.seq(); c != null; c = c.seq()) {
            if (c instanceof IChunkedSeq) {
                IChunkedSeq cs = (IChunkedSeq) c;
                if (!invokeXFChunk(cs.chunkedFirst())) {
                    break;
                }

                c = cs.chunkedMore();
            } else {
                if (!invokeXF(c)) {
                    break;
                }

                c = c.more();
            }

            if (!buf.isEmpty()) {
                this.s = c;
                return buf.toSeq(new LazySeq(this));
            }
        }

        invokeXF();
        return buf.isEmpty() ? null : buf.toTail();
    }

    public static class LongStep extends XFSeqStep {

        private final IFn.OLO xf;

        public LongStep(IFn xf, ISeq s, IXFSeqBuffer buf) {
            super(s, buf);
            this.xf = (IFn.OLO)xf;
        }

        @Override
        void invokeXF() {
            ((IFn)xf).invoke(buf);
        }

        @Override
        boolean invokeXF(ISeq s) {
            return buf == xf.invokePrim(buf, ((ILongSeq)s).firstLong());
        }

        @Override
        boolean invokeXFChunk(IChunk ch) {
            ILongChunk lch = (ILongChunk)ch;
            for (int i = 0; i < lch.count(); i++) {
                if (buf != xf.invokePrim(buf, lch.nthLong(i))) {
                    return false;
                }
            }
            return true;
        }
    }

    public static class DoubleStep extends XFSeqStep {

        private final IFn.ODO xf;

        public DoubleStep(IFn xf, ISeq s, IXFSeqBuffer buf) {
            super(s, buf);
            this.xf = (IFn.ODO)xf;
        }

        @Override
        void invokeXF() {
            ((IFn)xf).invoke(buf);
        }

        @Override
        boolean invokeXF(ISeq s) {
            return buf == xf.invokePrim(buf, ((IDoubleSeq)s).firstDouble());
        }

        @Override
        boolean invokeXFChunk(IChunk ch) {
            IDoubleChunk lch = (IDoubleChunk)ch;
            for (int i = 0; i < lch.count(); i++) {
                if (buf != xf.invokePrim(buf, lch.nthDouble(i))) {
                    return false;
                }
            }
            return true;
        }
    }

    public static class ObjectStep extends XFSeqStep {

        private final IFn xf;

        public ObjectStep(IFn xf, ISeq s, IXFSeqBuffer buf) {
            super(s, buf);
            this.xf = xf;
        }

        @Override
        void invokeXF() {
            xf.invoke(buf);
        }

        @Override
        boolean invokeXF(ISeq s) {
            return buf == xf.invoke(buf, s.first());
        }

        @Override
        boolean invokeXFChunk(IChunk ch) {
            for (int i = 0; i < ch.count(); i++) {
                if (buf != xf.invoke(buf, ch.nth(i))) {
                    return false;
                }
            }
            return true;
        }
    }
}
