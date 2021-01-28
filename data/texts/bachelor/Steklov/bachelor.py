from __future__ import division

import re
import sys
import random

from google.cloud import speech
from google.cloud.speech import enums
from google.cloud.speech import types

from google.cloud import texttospeech

import pyaudio
from six.moves import queue
import playsound


RATE = 16000
CHUNK = int(RATE / 10)

WAKE_WORD = "ёлка"

HALT = 1


class MicrophoneStream(object):

    def __init__(self, rate, chunk):
        self._rate = rate
        self._chunk = chunk

        self._buff = queue.Queue()
        self.closed = True

    def __enter__(self):
        self._audio_interface = pyaudio.PyAudio()
        self._audio_stream = self._audio_interface.open(
            format=pyaudio.paInt16,
            channels=1, rate=self._rate,
            input=True, frames_per_buffer=self._chunk,
            stream_callback=self._fill_buffer,
        )

        self.closed = False

        return self

    def __exit__(self, type, value, traceback):
        self._audio_stream.stop_stream()
        self._audio_stream.close()
        self.closed = True

        self._buff.put(None)
        self._audio_interface.terminate()

    def _fill_buffer(self, in_data, frame_count, time_info, status_flags):

        self._buff.put(in_data)
        return None, pyaudio.paContinue

    def generator(self):
        while not self.closed:

            chunk = self._buff.get()
            if chunk is None:
                return
            data = [chunk]

            while True:
                try:
                    chunk = self._buff.get(block=False)
                    if chunk is None:
                        return
                    data.append(chunk)
                except queue.Empty:
                    break

            yield b''.join(data)


def listen_print_loop(responses):

    num_chars_printed = 0
    for response in responses:
        if not response.results:
            continue

        result = response.results[0]
        if not result.alternatives:
            continue

        transcript = result.alternatives[0].transcript

        overwrite_chars = ' ' * (num_chars_printed - len(transcript))

        if not result.is_final:
            sys.stdout.write(transcript + overwrite_chars + '\r')
            sys.stdout.flush()

            num_chars_printed = len(transcript)

        else:
            transcript = transcript.lower()
            print(transcript + overwrite_chars)

            if WAKE_WORD in transcript: 
                command = transcript.split(WAKE_WORD,1)[1]
                process_command(command)


                if ("продублируй" in command):
                    print(command)
                    continue

                if ("выключись" in command) or ("выход" in command):
                    print("Exitting..")
                    break

            num_chars_printed = 0

        if (HALT==0):
            print("Exitting..")
            break


def process_command(text):
    global HALT
    if ("выключись" in text) or ("выход" in text):
        HALT = 0
        text_to_speech("выхожу")
        return

    if ("повтори" in text):
        text_to_speech(text)
        return

    if ("расскажи" in text) and (("что-нибудь" in text) or ("историю" in text)):
        random_respond = ["рандомный текст один", "рандомный текст два", "рандомный текст три", "бла бла бла"]

        text_to_speech(random.choice(random_respond))
        return

    if("расскажи о себе" in text) or ("ты кто" in text):
        respond = ["я ёлка", "я робот телеприсутствия", "бла бла бла", "больше текста богу текста"]
        text_to_speech(random.choice(respond))
        return


def text_to_speech(play_text):
    print(play_text)

    sound_filename = "output.mp3"

    client = texttospeech.TextToSpeechClient()
    synthesis_input = texttospeech.SynthesisInput(text=play_text)
    voice = texttospeech.VoiceSelectionParams(language_code="ru-RU", ssml_gender=texttospeech.SsmlVoiceGender.NEUTRAL)
    audio_config = texttospeech.AudioConfig(audio_encoding=texttospeech.AudioEncoding.MP3)
    response = client.synthesize_speech(input=synthesis_input, voice=voice, audio_config=audio_config)

    with open(sound_filename, "wb") as out:
        out.write(response.audio_content)
        print('Audio content written to file "output.mp3"')

    playsound.playsound(sound_filename)




def main():

    language_code = 'ru-RU'

    client = speech.SpeechClient()
    config = types.RecognitionConfig(encoding=enums.RecognitionConfig.AudioEncoding.LINEAR16, sample_rate_hertz=RATE, language_code=language_code)
    streaming_config = types.StreamingRecognitionConfig(config=config, interim_results=True)

    with MicrophoneStream(RATE, CHUNK) as stream:
        audio_generator = stream.generator()
        requests = (types.StreamingRecognizeRequest(audio_content=content) for content in audio_generator)

        responses = client.streaming_recognize(streaming_config, requests)

        print("ready")

        listen_print_loop(responses)


if __name__ == '__main__':
    main()
