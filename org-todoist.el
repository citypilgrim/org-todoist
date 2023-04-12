;;; -*- lexical-binding: t; -*-

(require 'dash)
(require 'org-agenda)

(defvar url-http-end-of-headers)  ; silence byte-compiler warnings

(defvar todoist-token
  (getenv "TODOIST_TOKEN"))

(defconst todoist-url
  "https://api.todoist.com/rest/v2")

(defvar todoist--cached-projects nil)

(defgroup todoist nil
  "Interface for todoist.com, a task tracking tool."
  :group 'extensions
  :group 'tools
  :link '(url-link :tag "Repository" "https://github.com/abrochard/emacs-todoist"))

(defcustom todoist-timeout nil
  "Timeout in second to reach todoist API."
  :group 'todoist
  :type 'number)

(defun todoist--query (method endpoint &optional data)
  "Main function to interact with Todoist api.

METHOD is http method string.
ENDPOINT is the endpoint string.
DATA is the request body."
  (let ((url (concat todoist-url endpoint))
        (url-request-method method)
        (url-request-extra-headers (append`(("Authorization" . ,(concat "Bearer " todoist-token)))
                                          (when data '(("Content-Type". "application/json")))))
        (url-request-data (if data
                              (encode-coding-string (json-encode data) 'utf-8)))
        (response nil))
    (with-current-buffer (url-retrieve-synchronously url nil nil todoist-timeout)
      (let ((status (todoist--parse-status-code)))
        (unless (string-match-p "2.." status)
          (throw 'bad-response (format "Bad status code returned: %s" status))))
      (goto-char url-http-end-of-headers)
      (setq response (unless (string-equal (buffer-substring (point) (point-max)) "\n") ;; no body
                       (json-read-from-string (decode-coding-region (point) (point-max) 'utf-8 t))))
      (kill-buffer (current-buffer)) ;; kill the buffer to free up some memory
      response)))

(defun todoist--parse-status-code ()
  "Parse the todoist response status code."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "HTTP/1.1 \\([0-9]\\{3\\}\\)")
    (match-string-no-properties 1)))

(defun todoist--task-id (task)
  "Get the task id.

TASK is the task object."
  (assoc-default 'id task))

(defun todoist--task-date (task)
  "Get the task due date.

TASK is the task object"
  (assoc-default 'date (assoc-default 'due task)))

(defun todoist--task-content (task)
  "Get the task content.

TASK is the task object."
  (assoc-default 'content task))

(defun todoist--task-project-id (task)
  "Get the task project id.

TASK is the task object"
  (assoc-default 'project_id task))

(defun todoist--project-id (project)
  "Get the project id.

PROJECT is the project object."
  (assoc-default 'id project))

(defun todoist--project-name (project)
  "Get the project name.

PROJECT is the project object."
  (assoc-default 'name project))

(defun todoist--task-description (task)
  "Get the description.

TASK is the task object"

    (assoc-default 'description task))

(defun todoist--filter-tasks (project tasks)
  "Get subset of tasks under a project.

PROJECT the project.
TASKS the list of tasks."
  (-filter (lambda (task)
             (equal (todoist--task-project-id task) (todoist--project-id project)))
           tasks))

(defun todoist--insert-heading (level str &optional todo)
  "Insert a org heading at a certain depth.

LEVEL is the level.
STR is the heading title.
TODO is optional to make this a todo heading."
  (goto-char (point-max))
  (if todo
      (insert (format "\n%s TODO %s" (make-string level ?*) str))
    (insert (format "\n%s %s" (make-string level ?*) str))))

(defun todoist--get-projects (&optional cache)
  "Get the list of all projects.

CACHE to read from cache rather than query upstream."
  (if cache
      todoist--cached-projects
    (setq todoist--cached-projects
          (append (todoist--query "GET" "/projects") nil))))

(defun todoist--get-tasks ()
  "Get the list of all tasks."
  (append (todoist--query "GET" "/tasks") nil))

(defun todoist--get-agenda-file (project)
  "Retrieve the corresponding `org-agenda` file for a PROJECT.

The function searches for an `org-agenda` file of the same name
 as the project.  If no file is found, it creates the file in
 the first directory found in `org-agenda-files`.  If no
directory is found, it creates the file in the same directory
as the `org-agenda` file"
  (let ((filename (concat (todoist--project-name project) ".org"))
        (result nil))
    ;; searching for file in org-agenda
    (dolist (dirorfile org-agenda-files)
      (when (not result)
        (if (file-accessible-directory-p dirorfile)
            (setq result (todoist--find-file-in-directory filename dirorfile))
          (and (string= (file-name-nondirectory dirorfile)
                        filename)
               (setq result dirorfile)))))
    result))

(defun todoist--find-file-in-directory (filename directory)
  "Recursively search for FILENAME in DIRECTORY and return its full path.
Returns the first match."
  (let ((result nil)
        (ignore '(".git" ".svn" "." "..")))
    (dolist (file (directory-files-and-attributes directory t) result)
      (when (not result)
        (let* ((name (car file))
               (fname (file-name-nondirectory name))
               (continue (not (member fname ignore)))
               (attrsp (eq t (cadr file))))
          (cond ((and continue
                      (string= filename fname))
                 (setq result name))
                ((and continue
                      attrsp)
                 (setq result (todoist--find-file-in-directory filename name)))))))))

(defun todoist--compare-tasks (tasks1 tasks2)
  "Computes the delta of tasks to go from TASKS1 to TASKS2.
Returns three sequences (created updated deleted)"
  (let ((result nil)
        (createdt '())
        (updatedt '())
        (deletedt '())
        (ids (cl-remove-if (lambda (x) (eq x nil)) (mapc #'todoist--task-id tasks1))))
    (dolist (t tasks2)
      (let ((tid (todoist--task-id t)))
        (cond (and (member tid ids)     ;update
                   (cl-remove tid ids)  ;to generate delete list
                   (push t updatedt))
              (push t createdt)         ;create
              )))
    (dolist (t tasks1)                  ;delete
      (if (member (todoist--task-id t) ids)
          (push t deletedt)))
    `(,createdt ,updatedt ,deletedt)))

(defun todoist--get-agenda-project-tasks (projectfile)
  "Gets the tasks within a project agenda file."
  ;; TODO implement
  ;; read all child headers in org file
  )

;; how do i retrieve all

(defun todoist--update-project-agenda (projectfile taskdeltas)
  "Update the project `org-agenda` PROJECTFILE with TASKDELTAS.

TASKDELTAS is a list of (CREATEDTASKS UPDATEDTASKS DELETEDTASKS)"
  ;; TODO implement
  )

(defun todoist--update-project-todoist (taskdeltas)
  "Update the project on todoist with TASKDELTAS."
  (cl-multiple-value-bind '(createdt updatedt deletedt) taskdeltas
    (mapc #'todoist--create-task createdt)
    (mapc #'todoist--update-task updatedt)
    (mapc #'todoist--delete-task deletedt)))

(defun todoist-update-agenda ()
  "Update `org-agenda-files` from tasks defined in todoist."
  (interactive)
  (let ((projects (todoist--get-projects))
        (tasks (todoist--get-tasks)))
    (dolist (p projects)
      (let* ((ptasks (todoist--get-agenda-project-tasks (todoist--get-agenda-file p)))
             (createdt nil)
             (updatedt nil)
             (deletedt nil))
        (cl-multiple-value-setq '(createdt updatedt deletedt)
          (todoist--compare-tasks ptasks (todoist--filter-tasks p tasks))))
      (todoist--update-project-agenda p `(,createdt ,updatedt ,deletedt)))))

(defun todoist--update-project-todoist (projectfile)
  "Update todoist project from tasks defined in `org-agenda-files`."
  ;; TODO implement
  ;; get todoist project id from file
  ;; get todoist project tasks with API
  ;; compare tasks and sync deltas   
  )

(defun todoist--project-id-from-file (projectfile)
  "Retrieves the `org-mode` project id property from PROJECTFILE."
  ;; TODO implement
  )

(defun todoist--tasks-from-file (projectfile)
  "Retrieve tasks from `org-agenda` PROJECTFILE."
  ;; TODO utilise org-collect-keywords to retrieve project ID
  
  )

;; (defun todoist-update-todoist ()
;;   "Update todoist from tasks defined in `org-agenda-files`."
;;   (interactive)
;;   (let ((projects (todoist--get-projects))
;;         (tasks (todoist--get-tasks)))
;;     (dolist (p projects)
;;       (let ((ptasks (todoist--get-agenda-project-tasks (todoist--get-agenda-file p)))
;;             (createtasks nil)
;;             (updatetasks nil)
;;             (deletetasks nil))
;;         (cl-multiple-value-setq '(createtasks updatetasks deletetasks)
;;           (todoist--get-task-crud (todoist--get) (todoist--filter-tasks p tasks)))
;;         (todoist--update-project-todoist p `(,createdt ,updatedt ,deletedt))))))

(provide 'org-todoist)
;;; org-todoist.el ends here
