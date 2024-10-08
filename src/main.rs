use inflectgr::{ConjugationProduct, Error, Number, Person, Stem, Tense, ThematicVerb, Verb};

fn main() -> Result<(), Error> {
    let ago = ThematicVerb::new(Stem::new("αγ".to_string())?)?;
    for tense in Tense::all() {
        for number in [Number::Singular, Number::Plural] {
            for person in Person::all() {
                println!("{tense:?} {person:?} {number:?}");
                let conjugated = ago.clone().conjugated(ConjugationProduct {
                    person,
                    number,
                    tense,
                    ..Default::default()
                });
                println!("{conjugated}");
            }
        }
    }

    Ok(())
}
