create table bug_comment_people asselect distinct person from(select b.author_id person from comment bunion select a.assigned_id person from bug a) corder by c.person /* Then edit the resulting table and createand identifer column.  */