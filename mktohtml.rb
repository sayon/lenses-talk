require 'github/markup'
file=ARGV[0]
print GitHub::Markup.render(file, File.read(file))
