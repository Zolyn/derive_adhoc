These three crates test forward/backward compatibility with the
version of derive-adhoc prior at the  most recent flag day.

I. Forward compatibility (higher layer crate is more up to date)

  new-b -> new derive_adhoc (bizarre version)
    |
    V
  old-a -> old derive_adhoc

This reuses pub-b.rs (which expects bizarre derive-adhoc)
and pub-a.rs.

2. Backward compatibility (lower layer crate is more up to date)

  old-b -> old derive_adhoc
    |
    V
  pub-a -> current derive_adhoc

This reuses pub-a.rs (which expects vanilla derive-adhoc)
and a filtered version of pub-b.rs called old-b.rs.
pub-b.rs expects bizarre derive-adhoc but there is no published
old version, so we have maint/update-bizarre filter out the _bizarres.
