
(** Output buffer of the printer, with whitespace buffering. Aligns the columns
    of the rows of [Core.vertically_aligned] documents, cells being possibly
    nested (cf. HACKING.md). *)
include Printing_buffer_intf.S
