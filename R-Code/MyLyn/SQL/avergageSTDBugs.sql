select avg(counter), std(counter) from (select bug_id, count(*) as counter from research_comment_comment group by bug_idorder by counter desc) a