SELECT source.event_author_id as asource, 
target.event_object_creator as atarget, 
sum(source.weight_in_minutes), 
b.Person_sakai_id as source_name,
b.Person_sakai_id as target_name,
count(*) 
from DW_Event_Fact source,
DW_Event_Fact target
, DW_Person_Dim b
where target.institution_id = 2
and source.institution_id = 2
and source.event_session >= 2
and target.event_object_creator = b.Person_id
and source.event_author_id = b.Person_id 
group by asource, atarget, source_name, target_name
order by source.event_session,
source.event_author_id