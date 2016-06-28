SELECT research_people_map.field1, 
	research_people_map.field2, 
	research_bug_fixer_people.assigned_name,
	research_bug_fixer_people.committer
FROM research_people_map INNER JOIN research_bug_fixer_people 
ON research_people_map.field2 = research_bug_fixer_people.assigned_id

SELECT research_people_map.field1, 
	research_people_map.field2, 
	research_committers.committerID
FROM research_people_map INNER JOIN research_committers 
ON research_people_map.field2 = research_committers.committerid


