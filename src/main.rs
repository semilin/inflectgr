use inflectgr::{
    ConjugationProduct, Error, Number, Person, Stem, Tense, ThematicVerb, Verb, Voice,
};

fn main() -> Result<(), Error> {
    let ago = ThematicVerb::new(Stem::new("αγ".to_string())?)?;
    for tense in Tense::all() {
        println!("{tense:?}");
        for voice in [Voice::Active, Voice::Middle] {
            println!("  {voice:?}");
            for number in [Number::Singular, Number::Plural] {
                for person in Person::all() {
                    let conjugated = ago.clone().conjugated(ConjugationProduct {
                        person,
                        number,
                        tense,
                        voice,
                        ..Default::default()
                    });
                    println!("    {person:?} {number:?}: {conjugated}");
                }
            }
        }
    }

    Ok(())
}
