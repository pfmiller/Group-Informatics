select * from research_comment_comment where author_id not in (select assigned_id from research_bug_fixer_people)