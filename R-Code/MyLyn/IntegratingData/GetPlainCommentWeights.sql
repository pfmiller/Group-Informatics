SELECT source.event_author_id as asource, 
source.event_object_creator as atarget, 
sum(source.weight_in_minutes),
count(*),
source.event_session 
from DW_Event_Fact source
where source.institution_id = 2
and source.event_session >= 2
and source.event_author_id is not NULL 
and source.event_object_creator is not NULL
group by asource, atarget
order by source.event_session,
source.event_author_id

-- ===add names ====
SELECT a.*, personA.Person_sakai_id as source_name,
personB.Person_sakai_id as target_name
FROM mylyn.research_buzilla_net_temp a,
DW_Person_Dim personA, 
DW_Person_Dim personB
WHERE
a.asource = personA.Person_id 
and a.atarget = personB.Person_id

--- get ids
select *, b.PersonID as source_id, 
c.PersonID as target_id from 
mylyn.research_buzilla_net_temp_b a,
mylyn.research_people_map b,
mylyn.research_people_map c
where 
a.source_name=b.assigned_id and
a.target_name=c.assigned_id
