{{/*
IMBoy Helm Chart —— 通用辅助模板
*/}}

{{/*
扩展的应用名称（支持 fullnameOverride）
*/}}
{{- define "imboy.name" -}}
{{- default .Chart.Name .Values.nameOverride | trunc 63 | trimSuffix "-" }}
{{- end }}

{{- define "imboy.fullname" -}}
{{- if .Values.fullnameOverride }}
{{- .Values.fullnameOverride | trunc 63 | trimSuffix "-" }}
{{- else }}
{{- $name := default .Chart.Name .Values.nameOverride }}
{{- if contains $name .Release.Name }}
{{- .Release.Name | trunc 63 | trimSuffix "-" }}
{{- else }}
{{- printf "%s-%s" .Release.Name $name | trunc 63 | trimSuffix "-" }}
{{- end }}
{{- end }}
{{- end }}

{{/*
Chart label（version 包含点，需转义为连字符）
*/}}
{{- define "imboy.chart" -}}
{{- printf "%s-%s" .Chart.Name .Chart.Version | replace "+" "_" | trunc 63 | trimSuffix "-" }}
{{- end }}

{{/*
通用标签
*/}}
{{- define "imboy.labels" -}}
helm.sh/chart: {{ include "imboy.chart" . }}
{{ include "imboy.selectorLabels" . }}
{{- if .Chart.AppVersion }}
app.kubernetes.io/version: {{ .Chart.AppVersion | quote }}
{{- end }}
app.kubernetes.io/managed-by: {{ .Release.Service }}
{{- end }}

{{/*
选择器标签（用于 matchLabels）
*/}}
{{- define "imboy.selectorLabels" -}}
app.kubernetes.io/name: {{ include "imboy.name" . }}
app.kubernetes.io/instance: {{ .Release.Name }}
{{- end }}

{{/*
后端专用选择器标签
*/}}
{{- define "imboy.backend.selectorLabels" -}}
app.kubernetes.io/name: {{ include "imboy.name" . }}-backend
app.kubernetes.io/instance: {{ .Release.Name }}
app.kubernetes.io/component: backend
{{- end }}

{{/*
Admin 前端专用选择器标签
*/}}
{{- define "imboy.admin.selectorLabels" -}}
app.kubernetes.io/name: {{ include "imboy.name" . }}-admin
app.kubernetes.io/instance: {{ .Release.Name }}
app.kubernetes.io/component: admin
{{- end }}

{{/*
ServiceAccount 名称
*/}}
{{- define "imboy.serviceAccountName" -}}
{{- if .Values.serviceAccount.create }}
{{- default (include "imboy.fullname" .) .Values.serviceAccount.name }}
{{- else }}
{{- default "default" .Values.serviceAccount.name }}
{{- end }}
{{- end }}

{{/*
镜像完整引用：支持 global.imageRegistry 前缀
*/}}
{{- define "imboy.image" -}}
{{- $registry := .global.imageRegistry | default "" }}
{{- $repo := .image.repository }}
{{- $tag := .image.tag | default "latest" }}
{{- if $registry }}
{{- printf "%s/%s:%s" $registry $repo $tag }}
{{- else }}
{{- printf "%s:%s" $repo $tag }}
{{- end }}
{{- end }}

{{/*
Secret 名称
*/}}
{{- define "imboy.secretName" -}}
{{- printf "%s-secrets" (include "imboy.fullname" .) }}
{{- end }}

{{/*
ConfigMap 名称
*/}}
{{- define "imboy.configMapName" -}}
{{- printf "%s-config" (include "imboy.fullname" .) }}
{{- end }}
