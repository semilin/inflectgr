use inflectgr::{ConjugationProduct, Error, Number, Person, Stem, Tense, ThematicVerb, Verb};

fn main() -> Result<(), Error> {
    let ago = ThematicVerb::new(Stem::new("αγ".to_string())?)?;
    for person in Person::all() {
        for number in [Number::Singular, Number::Plural] {
            let conjugated = ago.clone().conjugated(ConjugationProduct {
                person,
                number,
                ..Default::default()
            });
            println!("{conjugated}");
        }
    }
    Ok(())
}
