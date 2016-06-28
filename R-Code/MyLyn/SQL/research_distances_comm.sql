create table research_distances_comm as
select author_id, reporter_id, 
releaseId, count(*) as bug_comm from research_comment_by_release
where releaseId >=2
group by author_id, reporter_id, releaseId
order by releaseId

select * from research_distances_comm
order by releaseId