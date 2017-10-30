import os
import getpass
import imaplib
from email import message_from_bytes
import pdb
import argparse


def create_mail_connection(smtp_server, email, password):
    mail = imaplib.IMAP4_SSL(smtp_server)
    mail.login(email, password)
    return mail


def get_emails(mail, search='ALL', exclude_mids=None):
    resp = mail.search(None, search)
    check_response(resp)

    mid_list = resp[1][0].split()
    if exclude_mids:
        mid_list = [mid for mid in mid_list if mid.decode() not in exclude_mids]

    for mid in mid_list:
        yield (mid, fetch_email(mail, mid))


def fetch_email(mail, mid):
    resp = mail.fetch(mid, '(RFC822)')
    check_response(resp)
    msg = message_from_bytes(resp[1][0][1])
    return resp


def check_response(resp):
    if resp[0] != 'OK':
        raise Exception("Failed: " + str(resp))


READ_MID_FILE = '.read_mids.list'
def load_read_mids(folder):
    fname = os.path.join(folder, READ_MID_FILE)
    if not os.path.exists(fname):
        return set()
    with open(fname) as f:
        return set(l.strip() for l in f)


def save_read_mids(folder, mids):
    fname = os.path.join(folder, READ_MID_FILE)
    with open(fname, 'w') as f:
        for mid in mids:
            f.write(mid)
            f.write('\n')


def download_email_items(mail, folder):
    if not os.path.exists(folder):
        os.makedirs(folder)

    read_mids = load_read_mids(folder)
    try:
        for mid, email in get_emails(mail, exclude_mids=read_mids):
            print(mid)
            # Do Something
            read_mids.add(mid.decode())
    finally:
        save_read_mids(folder, read_mids)


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--email', type=str, required=True)
    parser.add_argument('--folder', type=str, required=True)
    parser.add_argument('--mailbox', type=str, default='inbox')
    parser.add_argument('--server', type=str, default='imap.gmail.com')
    parser.add_argument('--port', type=int, default=993)
    args = parser.parse_args()

    smtp_server = args.server
    smtp_port = args.port
    email_address = args.email
    password = getpass.getpass(prompt='Password: ')
    folder = folder = os.path.join(args.folder, args.mailbox)

    mail = create_mail_connection(smtp_server, email_address, password)
    mail.select(args.mailbox)
    download_email_items(mail, folder)
