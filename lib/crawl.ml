open Util ;;    
open CrawlerServices ;;
open Order ;;
open Pagerank ;;


module MoogleRanker
  = InDegreeRanker (PageGraph) (PageScore)
  (*
     = RandomWalkRanker (PageGraph) (PageScore) (struct 
       let do_random_jumps = Some 0.20
       let num_steps = 1000
     end)
  *)

(* Dictionaries mapping words (strings) to sets of crawler links *)
module WordDict = Dict.Make(
  struct 
    type key = string
    type value = LinkSet.set
    let compare = string_compare
    let string_of_key = (fun s -> s)
    let string_of_value = LinkSet.string_of_set

    (* These functions are for testing purposes *)
    let gen_key () = ""
    let gen_key_gt x () = gen_key ()
    let gen_key_lt x () = gen_key ()
    let gen_key_random () = gen_key ()
    let gen_key_between x y () = None
    let gen_value () = LinkSet.empty
    let gen_pair () = (gen_key(),gen_value())
  end)

(* A query module that uses LinkSet and WordDict *)
module Q = Query.Query(
  struct
    module S = LinkSet
    module D = WordDict
  end)

let print s = 
  let _ = Printf.printf "%s\n" s in
  flush_all();;


(***********************************************************************)
(*    PART 1: CRAWLER                                                  *)
(***********************************************************************)

(* TODO: Build an index as follows:
 * 
 * Remove a link from the frontier (the set of links that have yet to
 * be visited), visit this link, add its outgoing links to the
 * frontier, and update the index so that all words on this page are
 * mapped to linksets containing this url.
 *
 * Keep crawling until we've
 * reached the maximum number of links (n) or the frontier is empty. *)
let rec crawl (n:int) (frontier: LinkSet.set)
    (visited : LinkSet.set) (d:WordDict.dict) : WordDict.dict = 
  WordDict.empty
;;

let rec crawl (n:int) (frontier: LinkSet.set)
    (visited : LinkSet.set) (d:WordDict.dict) : WordDict.dict = 
  if n <= 0 || LinkSet.is_empty frontier then
    (* base: reached max pages or no more links to visit *)
    d
  else
    (* remove link from the frontier *)
    match LinkSet.choose frontier with
    | None -> d  (* shouldn't happen since we checked is_empty, but in case *)
    | Some (link, frontier') ->
        if LinkSet.member visited link then
          (* already visited this link, skip *)
          crawl n frontier' visited d
        else
          (* visit link *)
          match CrawlerServices.get_page link with
          | None ->
              (* couldn't fetch the page, skip *)
              crawl n frontier' visited d
          | Some page ->
              (* add link to visited *)
              let visited' = LinkSet.insert link visited in
              (* update index w/ words from page *)
              let d' = List.fold_left (fun acc word ->
                          let current_links = match WordDict.lookup acc word with
                            | None -> LinkSet.empty
                            | Some s -> s
                          in
                          let updated_links = LinkSet.insert link current_links in
                          WordDict.insert acc word updated_links
                        ) d page.words
              in
              (* add new links from page to frontier *)
              let new_links = List.fold_left (fun acc lnk ->
                               if LinkSet.member visited' lnk || LinkSet.member frontier' lnk then
                                 acc
                               else
                                 LinkSet.insert lnk acc
                             ) frontier' page.links
              in
              (* recurse w/ updated parameters *)
              crawl (n - 1) new_links visited' d'
;;

let crawler () = 
  crawl num_pages_to_search (LinkSet.singleton initial_link) LinkSet.empty
    WordDict.empty
;;

(* Debugging note: if you set debug=true in moogle.ml, it will print out your
 * index after crawling. *)
