def column-exists? [name] { $name in ($in | columns) }

def on-commons? [] {
    let search = ({
        action: query,
        format: json,
        prop: imageinfo,
        titles: $"File:($in)"
    } | to url)
    let url = $"https://commons.wikimedia.org/w/api.php?($search)"
    let response = (curl -s $url | from json)
    if ($response | column-exists? query) {
        if ($response.query | column-exists? pages) {
            not ($response.query.pages | column-exists? "-1")
        } else {
            false
        }
    } else {
        false
    }
}

for file in (ls /media/external-data/Pictures/Wikimedia) {
    if not ($file.name | path basename | on-commons?) {
        mv $file.name ~/Downloads
        ^echo $"✅ Moved ($file.name) to ~/Downloads"
    } else {
        ^echo $"❌ ($file.name) is on commons"
    }
}

for file in (ls ~/Downloads) {
    if ($file.name | path basename | on-commons?) {
        mv $file.name /media/external-data/Picutres/Wikimedia
        ^echo $"✅ Moved ($file.name) to /media/external-data/Pictures/Wikimedia"
    } else {
        ^echo $"❌ ($file.name) is not on commons"
    }
}
