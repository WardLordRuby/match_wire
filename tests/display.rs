#[cfg(test)]
mod tests {
    use match_wire::utils::display::Line;

    #[test]
    fn test_line_len() {
        for i in 0..=512 {
            let line = format!("{}", Line(i));
            assert_eq!(line.chars().count(), i);
        }
    }
}
