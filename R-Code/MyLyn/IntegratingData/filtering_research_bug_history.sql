select * from research_bug_history
where from_dev not in('pde-ui-inbox','Platform-UI-Inbox', 'platform-cvs-inbox','mylar-inbox','mylyn-triaged',
'platform-doc-inbox','platform-resources-inbox','platform-ui-triaged','AJDT-inbox','platform-compare-inbox'
,'mylyn-inbox','gmt-oaw-inbox','equinox.framework-inbox','community.articles-inbox','platform-ide-inbox')
and to_dev not in('pde-ui-inbox','Platform-UI-Inbox', 'platform-cvs-inbox',
'mylar-inbox','mylyn-triaged','platform-doc-inbox','platform-resources-inbox','platform-ui-triaged','AJDT-inbox'
,'platform-compare-inbox','mylyn-inbox','gmt-oaw-inbox','equinox.framework-inbox','community.articles-inbox','platform-ide-inbox')
