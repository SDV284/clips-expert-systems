# Лабораторна робота 1

**Тема:** Операції з фактами (assert, retract, facts) та прості правила.

## Файли

- `all_tasks.clp` – усі завдання роботи в одному файлі;
- `facts.txt` – список фактів для завантаження командою load-facts;
- `task2_deffacts.clp` – формування бази фактів конструкцією deffacts;
- `task3_rule.clp` – просте правило виведення;
- `task4_template.clp` – шаблон person (deftemplate) з простими слотами;
- `task4_advanced.clp` – шаблон з multislot, type та allowed-values;
- `find-men.clp` – правило пошуку чоловіків;
- `find-woman.clp` – правило пошуку жінок;
- `find-by-name-and-age.clp` – правила пошуку за ім'ям та віком.

### Запуск

```
CLIPS> (clear)
CLIPS> (load "all_tasks.clp")
CLIPS> (reset)
CLIPS> (run)
```

> Файли `find-*.clp` містять лише правила, тому перед ними слід завантажити шаблон `person` з `task4_advanced.clp`.
