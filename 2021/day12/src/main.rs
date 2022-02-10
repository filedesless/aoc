use std::collections::HashMap;
use std::collections::HashSet;

type Node<'a> = &'a str;
/// Nodes and the nodes they lead to
type Graph<'a> = HashMap<Node<'a>, HashSet<Node<'a>>>;
type Path<'a> = Vec<&'a str>;

fn parse(content: &str) -> Graph {
    let mut graph: Graph = HashMap::new();
    for line in content.lines() {
        let mut split = line.split('-');
        let a = split.next().unwrap();
        let b = split.next().unwrap();
        graph.entry(a).or_default().insert(b);
        graph.entry(b).or_default().insert(a);
    }

    graph
}

fn big(s: &str) -> bool {
    s.chars().next().unwrap().is_uppercase()
}

fn dfs<'a>(
    graph: &Graph<'a>,
    path: &mut Path<'a>,
    src: &Node<'a>,
    dst: &Node<'a>,
) -> Vec<Path<'a>> {
    if src == dst {
        vec![path.to_vec()]
    } else {
        let adjacents = &graph[src];
        let mut paths = vec![];
        for adjacent in adjacents {
            let visited = path.contains(adjacent);
            if !visited || big(adjacent) {
                path.push(adjacent);
                paths.append(&mut dfs(graph, path, adjacent, dst));
                path.pop();
            }
        }
        paths
    }
}

fn dfs_b<'a>(
    graph: &Graph<'a>,
    path: &mut Path<'a>,
    src: &Node<'a>,
    dst: &Node<'a>,
    can_go: bool,
) -> Vec<Path<'a>> {
    if src == dst {
        vec![path.clone()]
    } else {
        let adjacents = &graph[src];
        let mut paths = vec![];
        for adjacent in adjacents.iter().filter(|&&adj| adj != "start") {
            let visitable = !path.contains(adjacent) || big(adjacent);
            if can_go || visitable {
                path.push(adjacent);
                paths.append(&mut dfs_b(graph, path, adjacent, dst, can_go && visitable));
                path.pop();
            }
        }
        paths
    }
}
fn paths<'a>(graph: &'a Graph) -> Vec<Path<'a>> {
    let mut path = vec!["start"];
    dfs(graph, &mut path, &"start", &"end")
}

fn paths_b<'a>(graph: &'a Graph) -> Vec<Path<'a>> {
    let mut path = vec!["start"];
    dfs_b(graph, &mut path, &"start", &"end", true)
}

fn day12a(content: &str) -> usize {
    let graph = parse(content);
    paths(&graph).len()
}

fn day12b(content: &str) -> usize {
    let graph = parse(content);
    paths_b(&graph).len()
}

fn main() {
    let content = include_str!("input12a.txt");
    println!("day12a: {}", day12a(content));
    println!("day12b: {}", day12b(content));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let content = include_str!("test12a.txt");
        let graph = parse(content);

        assert_eq!(6, graph.len());
        assert_eq!(2, graph.get("start").unwrap().len());
        assert_eq!(2, graph.get("end").unwrap().len());
        assert_eq!(4, graph.get("A").unwrap().len());
        assert_eq!(1, graph.get("c").unwrap().len());
    }

    #[test]
    fn test_paths() {
        let content = include_str!("test12a.txt");
        let graph = parse(content);
        assert_eq!(10, paths(&graph).len());
        let content = include_str!("test12a_larger.txt");
        let graph = parse(content);
        assert_eq!(19, paths(&graph).len());
        let content = include_str!("test12a_largest.txt");
        let graph = parse(content);
        assert_eq!(226, paths(&graph).len());
    }

    #[test]
    fn test_paths_b() {
        let content = include_str!("test12a.txt");
        let graph = parse(content);
        assert_eq!(36, paths_b(&graph,).len());
        let content = include_str!("test12a_larger.txt");
        let graph = parse(content);
        assert_eq!(103, paths_b(&graph,).len());
        let content = include_str!("test12a_largest.txt");
        let graph = parse(content);
        assert_eq!(3509, paths_b(&graph,).len());
    }

    #[test]
    fn test_day12b() {
        let content = include_str!("input12a.txt");
        assert_eq!(122880, day12b(content));
    }
}
