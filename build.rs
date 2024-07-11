use std::error::Error;

use vergen_gitcl::{Emitter, GitclBuilder};

fn main() -> Result<(), Box<dyn Error>> {
    Emitter::default()
        .add_instructions(
            &GitclBuilder::default()
                .sha(false)
                .commit_message(true)
                .build()?,
        )?
        .emit()?;

    Ok(())
}
