main() {
    DIRECTORY="~/multi-term-plus"
    if [ ! -d "$DIRECTORY" ]; then
        git clone https://github.com/aborn/multi-term-plus.git $DIRECTORY
        echo "Add following code to your emacs init file:"
        echo " (add-to-list 'load-path \"~/multi-term-plus\")"
        echo " (require 'multi-term-config)"
    else
        echo "direcotry ${DIRECTORY} already exists."
    fi
}

main
