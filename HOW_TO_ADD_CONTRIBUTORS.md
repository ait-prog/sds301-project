# Как добавить контрибьюторов на GitHub

Чтобы аватары всех участников команды отображались в разделе **Contributors** на GitHub, каждый участник должен:

## Шаг 1: Настроить Git с вашим GitHub email

1. Проверьте текущий email:
```bash
git config user.email
```

2. Установите email, который привязан к вашему GitHub аккаунту:
```bash
git config user.email "your-github-email@example.com"
git config user.name "Your Name"
```

> **Важно:** Email должен совпадать с email в настройках вашего GitHub аккаунта (Settings → Emails)

## Шаг 2: Сделать коммит

Каждый участник должен сделать хотя бы один коммит. Например:

1. Клонируйте репозиторий:
```bash
git clone https://github.com/ait-prog/sds301-project.git
cd sds301-project
```

2. Сделайте небольшое изменение (например, добавьте комментарий в README.md)

3. Закоммитьте и запушьте:
```bash
git add .
git commit -m "Add contribution by [Your Name]"
git push
```

## Альтернативный способ (для уже существующих коммитов)

Если коммиты уже сделаны с неправильным email, можно переписать историю:

```bash
git filter-branch --env-filter '
OLD_EMAIL="old-email@example.com"
CORRECT_NAME="Your Name"
CORRECT_EMAIL="your-github-email@example.com"

if [ "$GIT_COMMITTER_EMAIL" = "$OLD_EMAIL" ]
then
    export GIT_COMMITTER_NAME="$CORRECT_NAME"
    export GIT_COMMITTER_EMAIL="$CORRECT_EMAIL"
fi
if [ "$GIT_AUTHOR_EMAIL" = "$OLD_EMAIL" ]
then
    export GIT_AUTHOR_NAME="$CORRECT_NAME"
    export GIT_AUTHOR_EMAIL="$CORRECT_EMAIL"
fi
' --tag-name-filter cat -- --branches --tags
```

## Проверка

После того, как все участники сделают коммиты, раздел Contributors на GitHub автоматически обновится и покажет всех участников с их аватарами.

## Участники команды

- **Yerkezhan Burambay**
- **Zhanahmetov Ansar**
- **Ruslan Kudaibergenov**
- **Alibek Aitbekov**

