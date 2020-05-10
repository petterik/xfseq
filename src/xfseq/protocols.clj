(ns xfseq.protocols)

(defprotocol IDeconstruct
  (deconstruct! [this]
    "Safely returns its parts if they haven't been used
     already, rendering the object unusable.

     Returns nil if they're already been used.

     This is really only meant to be used with xfseq.core/consume."))

(defprotocol ILongSeqable
  (long-seq [this]))

(defprotocol IDoubleSeqable
  (double-seq [this]))
