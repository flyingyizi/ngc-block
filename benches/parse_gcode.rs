use criterion::{black_box, criterion_group, criterion_main, Criterion};

use ngc_block::{parse_lines, GCodes};

fn criterion_benchmark(c: &mut Criterion) {
    let doc = "(;Filament used: 0.943758m)";
    let g1 = "G1 X132.273 Y137.397";
    let mut pest_group = c.benchmark_group("pest");
    pest_group.bench_function("parse_1", |b| {
        b.iter(|| {
            let _ = parse_lines(black_box(doc));
        })
    });
    pest_group.bench_function("parse_2", |b| {
        b.iter(|| {
            let ret = parse_lines(black_box(g1));
            // "N9 X-40.0 g_modes:{GCodeMotion: G1}
            if let Ok(vec) = ret {
                let first = vec.get(0).unwrap();

                assert_eq!(first.n_number, None);
                assert_eq!(first.g_modes[&ngc_block::GGroup::GCodeMotion], GCodes::G1);
                assert_eq!(first.x_number, Some(132.273));
                assert_eq!(first.y_number, Some(137.397));
            } else {
                panic!("Expected a G1");
            }
        })
    });

    pest_group.finish();

}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
